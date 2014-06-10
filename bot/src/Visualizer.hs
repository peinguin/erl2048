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
init (App conn _ stateBest _ skey) = do
  clazz <- newClass [
    defPropertySigRO' "result" skey (\_ ->
            readIORef stateBest),
        defMethod' "factorial" (\obj txt -> do
            let n = read $ Text.unpack txt :: Integer
            writeIORef stateBest $ Text.pack "Working..."
            fireSignal skey obj
            forkIO $ do
                let out = Text.take 1000 . Text.pack . show $ product [1..n]
                evaluate out
                writeIORef stateBest out
                fireSignal skey obj
            return ())]
  ctx <- newObject clazz ()
  doc <- getDataFileName "main.qml"
  runEngineLoop defaultEngineConfig {
  initialDocument = fileDocument doc,
  contextObject = Just $ anyObjRef ctx}
