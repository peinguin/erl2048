module Visualizer
(
  Visualizer.init
)
where

import Graphics.QML
import Data.IORef
import qualified Data.Text as Text
import Control.Concurrent
import Control.Exception

import Paths_2048bot
import Types

init :: App -> IO ()
init (App _ stateBest stateScore _ skey) = do
  clazz <- newClass [
    defPropertySigRO' "best" skey (\_ ->
                                    readIORef stateBest),
    defPropertySigRO' "score" skey (\_ ->
                                    readIORef stateScore)]
  ctx <- newObject clazz ()
  doc <- getDataFileName "main.qml"
  runEngineLoop defaultEngineConfig {
  initialDocument = fileDocument doc,
  contextObject = Just $ anyObjRef ctx}
