{-# LANGUAGE TypeFamilies #-}
module Visualizer
(
  Visualizer.init,
  Visualizer.redrawScores,
  Visualizer.redrawBest,
  Visualizer.redrawGrid
)
where

import qualified Graphics.QML as QML
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Control.Concurrent as Concurrent

import Paths_2048bot
import Types

instance QML.Marshal Cell where
    type MarshalMode Cell c d = QML.ModeObjFrom Cell c
    marshaller = QML.fromMarshaller QML.fromObjRef

instance QML.DefaultClass Cell where
  classMembers = [
    QML.defPropertyRO "value" $ (\(Cell value _ _) -> return value)]

init :: IO (
  QML.ObjRef (),
  QML.SignalKey (IO()),
  IORef.IORef Text.Text,
  IORef.IORef Text.Text,
  IORef.IORef Text.Text)
init = do
  -- display references
  stateBest  <- IORef.newIORef $ Text.pack "0"
  stateScore <- IORef.newIORef $ Text.pack "0"
  stateGrid  <- IORef.newIORef $ Text.pack ""
  skey   <- QML.newSignalKey
  
  clazz <- QML.newClass [
    QML.defPropertySigRO' "best" skey (\_ ->
                                    IORef.readIORef stateBest),
    QML.defPropertySigRO' "score" skey (\_ ->
                                    IORef.readIORef stateScore),
    QML.defSignal  "grid" skey]
           
  ctx <- QML.newObject clazz ()
  doc <- getDataFileName "main.qml"
  Concurrent.forkIO $ QML.runEngineLoop QML.defaultEngineConfig {
    QML.initialDocument = QML.fileDocument doc,
    QML.contextObject = Just $ QML.anyObjRef ctx}
  return (ctx, skey, stateBest, stateScore, stateGrid)

redrawScores :: App -> [Char] -> IO ()
redrawScores (App _ ctx skey _ stateScore _) val = do
  print $ QML.fromObjRef ctx
  IORef.writeIORef stateScore (Text.pack val)
  QML.fireSignal skey (QML.anyObjRef ctx)

redrawBest :: App -> [Char] -> IO ()
redrawBest (App _ ctx skey stateBest _ _) val = do
  IORef.writeIORef stateBest (Text.pack val)
  QML.fireSignal skey (QML.anyObjRef ctx)

redrawGrid :: App -> [[Maybe Cell]] -> IO ()
redrawGrid (App _ ctx skey _ _ stateGrid) _ = do
  IORef.writeIORef stateGrid $ Text.pack "0"
  QML.fireSignal skey (QML.anyObjRef ctx)
