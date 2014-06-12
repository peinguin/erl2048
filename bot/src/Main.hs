module Main ( main ) where

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import Types
import qualified Visualizer as V

main :: IO ()
main = WS.runClient "127.0.0.1" 8081 "/websocket" app

app :: WS.ClientApp ()
app conn = do
  (ctx, skey, stateBest, stateScore, stateGrid) <- V.init
  let app = App conn ctx skey stateBest stateScore stateGrid
  
  WS.sendTextData conn (Aeson.encode Action {action = "start", value = Nothing })
  loop app
  WS.sendClose conn ( Text.pack "Bye!" )

loop :: App -> IO ()
loop app = do
  let (App conn _ _ _ _ _) = app
  res <- WS.receiveData conn
  let game = Aeson.decode res :: Maybe Game
  let act = decision (game)

  updateGui app game

  case act of
    Nothing -> return ()
    Just act -> WS.sendTextData conn (Aeson.encode act) >> loop app

decision :: Maybe Game -> Maybe Action
decision _ = Nothing

updateGui :: App -> Maybe Game -> IO ()
updateGui _ Nothing = return ()
updateGui app (Just game) = do
  V.redrawScores app . show $ currScore game
  V.redrawBest app . show $ getBest $ grid game
  V.redrawGrid app $ grid game
