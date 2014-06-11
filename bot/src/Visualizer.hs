module Visualizer
(
  Visualizer.init,
  Visualizer.redrawScores,
  Visualizer.redrawBest
)
where

import qualified Graphics.QML as QML
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Control.Concurrent as Concurrent

import Paths_2048bot
import Types

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
  skey <- QML.newSignalKey
  
  clazz <- QML.newClass [
    QML.defPropertySigRO' "best" skey (\_ ->
                                    IORef.readIORef stateBest),
    QML.defPropertySigRO' "score" skey (\_ ->
                                    IORef.readIORef stateScore)]
           
  ctx <- QML.newObject clazz ()
  doc <- getDataFileName "main.qml"
  Concurrent.forkIO $ QML.runEngineLoop QML.defaultEngineConfig {
    QML.initialDocument = QML.fileDocument doc,
    QML.contextObject = Just $ QML.anyObjRef ctx}
  return (ctx, skey, stateBest, stateScore, stateGrid)

redrawScores :: App -> [Char] -> IO ()
redrawScores (App _ ctx skey _ stateScore _) val = do
  IORef.writeIORef stateScore (Text.pack val)
  QML.fireSignal skey (QML.anyObjRef ctx)

redrawBest :: App -> [Char] -> IO ()
redrawBest (App _ ctx skey stateBest _ _) val = do
  IORef.writeIORef stateBest (Text.pack val)
  QML.fireSignal skey (QML.anyObjRef ctx)
