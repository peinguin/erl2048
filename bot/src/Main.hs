module Main ( main ) where

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import Types

main :: IO ()
main = WS.runClient "127.0.0.1" 8081 "/websocket" app

app :: WS.ClientApp ()
app conn = do
	WS.sendTextData conn (Aeson.encode Action {action = "start", value = Nothing })
        
        loop conn
        
	WS.sendClose conn ( Text.pack "Bye!" )

loop :: WS.ClientApp ()
loop conn = do
  res <- WS.receiveData conn
  let act = decision (Aeson.decode res :: Maybe Game)
  case act of
    Nothing -> return ()
    Just act -> WS.sendTextData conn (Aeson.encode act) >> loop conn
                     
decision :: Maybe Game -> Maybe Action
decision _ = Nothing