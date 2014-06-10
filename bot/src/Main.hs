module Main ( main ) where

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as Concurrent
import qualified Data.IORef as IORef
import qualified Graphics.QML as QML

import Types
import qualified Visualizer as V

main :: IO ()
main = WS.runClient "127.0.0.1" 8081 "/websocket" app

app :: WS.ClientApp ()
app conn = do
  -- display references
  stateBest  <- IORef.newIORef $ Text.pack "0"
  stateScore <- IORef.newIORef $ Text.pack "0"
  stateGrid  <- IORef.newIORef $ Text.pack ""
  skey <- QML.newSignalKey
  
  let app = App conn stateBest stateScore stateGrid skey
  
  Concurrent.forkIO $ V.init app
  WS.sendTextData conn (Aeson.encode Action {action = "start", value = Nothing })
  loop app
  WS.sendClose conn ( Text.pack "Bye!" )

loop :: App -> IO ()
loop app = do
  let (App conn _ stateScore _ _) = app
  res <- WS.receiveData conn
  let game = Aeson.decode res :: Maybe Game
  let act = decision (game)
  updateScores app
  case act of
    Nothing -> return ()
    Just act -> WS.sendTextData conn (Aeson.encode act) >> loop app
                     
decision :: Maybe Game -> Maybe Action
decision _ = Nothing

updateScores :: App -> IO ()
updateScores (App _ _ stateScore _ skey) = do
  putStrLn "Hello, World!"
  IORef.writeIORef stateScore (Text.pack "100")
  QML.fireSignal skey
