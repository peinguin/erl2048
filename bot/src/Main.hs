module Main ( main ) where

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import Data.IORef
import Graphics.QML

import Types
import qualified Visualizer as V

main :: IO ()
main = WS.runClient "127.0.0.1" 8081 "/websocket" app

app :: WS.ClientApp ()
app conn = do
  -- display references
  stateBest  <- newIORef $ Text.pack ""
  stateScore <- newIORef $ Text.pack ""
  stateGrid  <- newIORef $ Text.pack ""
  skey <- newSignalKey
  
  let app = App conn stateBest stateScore stateGrid skey
  
  V.init app
  WS.sendTextData conn (Aeson.encode Action {action = "start", value = Nothing })
  loop app
  WS.sendClose conn ( Text.pack "Bye!" )

loop :: App -> IO ()
loop app = do
  let (App conn _ stateScore _ _) = app
  res <- WS.receiveData conn
  let game = Aeson.decode res :: Maybe Game
  let act = decision (game)
  updateScores stateScore game
  case act of
    Nothing -> return ()
    Just act -> WS.sendTextData conn (Aeson.encode act) >> loop app
                     
decision :: Maybe Game -> Maybe Action
decision _ = Nothing

updateScores :: IORef Text.Text -> Maybe Game -> IO ()
updateScores stateScore _ = do
  writeIORef stateScore (Text.pack "100")
