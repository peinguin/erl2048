{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, TypeFamilies #-}
module Visualizer
(
  Visualizer.init,
  Visualizer.QMLGame(..),
  Visualizer.update
)
where

import qualified Graphics.QML as QML
import qualified Control.Concurrent as Concurrent
import qualified Data.IORef as IORef
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable
import Paths_2048bot
import qualified Types as T

init :: IO (QML.ObjRef QMLGame)
init = do
  stateScores <- IORef.newIORef 0
  stateGrid   <- IORef.newIORef []
  let sqlgame = QMLGame stateScores stateGrid
  game <- QML.newObjectDC sqlgame 
  doc <- getDataFileName "main.qml"
  Concurrent.forkIO $ QML.runEngineLoop QML.defaultEngineConfig {
    QML.initialDocument = QML.fileDocument doc,
    QML.contextObject = Just $ QML.anyObjRef  game}
  return game

processGrid :: [[Maybe T.Cell]] -> IO [[Maybe (QML.ObjRef T.Cell)]]
processGrid [] = return []
processGrid [x] = do
  clearX <- processRow x
  return[clearX]
processGrid (x:xs) = do
  clearX <- processGrid [x]
  clearXS <- processGrid xs
  return $ clearX ++ clearXS

processRow :: [Maybe T.Cell] -> IO [Maybe (QML.ObjRef T.Cell)]
processRow [] = return []
processRow [x] = do
  clearX <- processCell x
  return [clearX]
processRow (x:xs) = do
  clearX <- processRow [x]
  clearXS <- processRow xs
  return $ clearX ++ clearXS

processCell :: Maybe T.Cell -> IO (Maybe (QML.ObjRef T.Cell))
processCell Nothing = return Nothing
processCell (Just cell) = do
  cellRef <- QML.newObjectDC cell
  return $ Just cellRef

getGrid :: QMLGame -> IO [[Maybe (QML.ObjRef T.Cell)]]
getGrid ( QMLGame _ gridRef ) = do
  grid <- (IORef.readIORef gridRef )
  processGrid grid

update :: QML.ObjRef QMLGame -> Maybe T.Game -> IO()
update _ Nothing = return ()
update qmlgameRef (Just (T.Game _ scores _ _ _ _ grid)) = do
  let qmlgame = QML.fromObjRef qmlgameRef
  let (QMLGame scoresRef gridRef) = qmlgame
  IORef.writeIORef scoresRef scores
  IORef.writeIORef gridRef grid
  QML.fireSignal (Proxy.Proxy :: Proxy.Proxy TheSignal) qmlgameRef


processMergedFrom :: [T.Cell] -> IO [QML.ObjRef T.Cell]
processMergedFrom [] = error "No Way!"
processMergedFrom [x] =  do
  cellRef <- QML.newObjectDC x
  return [cellRef]
processMergedFrom (x:xs) = do
  clearX <- processMergedFrom [x]
  clearXS <- processMergedFrom xs
  return $ clearX ++ clearXS

getMergedFrom :: Maybe [T.Cell] -> IO (Maybe [QML.ObjRef T.Cell])
getMergedFrom Nothing = return Nothing
getMergedFrom (Just cells) = do
  processed <- processMergedFrom cells
  return $ Just processed

getPreviousPosition ::
    Maybe T.PreviousPosition -> IO (Maybe (QML.ObjRef T.PreviousPosition))
getPreviousPosition Nothing = return Nothing
getPreviousPosition (Just ppos) = do
  pposRef <- QML.newObjectDC ppos
  return $ Just pposRef

data QMLGame = QMLGame
               (IORef.IORef Int)
               (IORef.IORef [[Maybe T.Cell]])
  deriving (Typeable.Typeable)

data TheSignal deriving Typeable.Typeable
instance QML.SignalKeyClass TheSignal where
    type SignalParams TheSignal = IO ()        

instance QML.Marshal T.Cell where
    type MarshalMode T.Cell c d = QML.ModeObjFrom T.Cell c
    marshaller = QML.fromMarshaller QML.fromObjRef

instance QML.Marshal T.PreviousPosition where
    type MarshalMode T.PreviousPosition c d = QML.ModeObjFrom T.PreviousPosition c
    marshaller = QML.fromMarshaller QML.fromObjRef

instance QML.DefaultClass T.PreviousPosition where
    classMembers = [
       QML.defPropertyRO "x" $ (\(T.PreviousPosition x _) -> return x),
       QML.defPropertyRO "y" $ (\(T.PreviousPosition _ y) -> return y)]

instance QML.DefaultClass T.Cell where
  classMembers = [
    QML.defPropertyRO "value"            $ (\(T.Cell value _ _) -> return value),
    QML.defPropertyRO "mergedFrom"       $
           (\(T.Cell _ mergedFrom _) -> getMergedFrom mergedFrom),
    QML.defPropertyRO "previousPosition" $
           (\(T.Cell _ _ previousPosition) -> getPreviousPosition previousPosition)]

instance QML.Marshal QMLGame where
    type MarshalMode QMLGame c d = QML.ModeObjFrom QMLGame c
    marshaller = QML.fromMarshaller QML.fromObjRef

instance QML.DefaultClass QMLGame where
  classMembers = [
    QML.defPropertySigRO
      "score"
      (Proxy.Proxy :: Proxy.Proxy TheSignal)
      (\(QMLGame currScore _) -> return =<< IORef.readIORef currScore),
    QML.defPropertySigRO "grid"  (Proxy.Proxy :: Proxy.Proxy TheSignal) getGrid,
      QML.defPropertySigRO "best"
      (Proxy.Proxy :: Proxy.Proxy TheSignal)
      (\game -> do
        let (QMLGame _ gridRef) = game
        grid <- IORef.readIORef gridRef
        return $ T.getBest grid),
    QML.defSignal "updateSignal" (Proxy.Proxy :: Proxy.Proxy TheSignal)]

