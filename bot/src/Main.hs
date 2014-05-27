module Main ( main ) where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Control.Applicative ((<$>), (<*>), empty)
import qualified Data.Text as Text

data Action = Action { action :: String, value :: String}
	 deriving (Show)

instance ToJSON Action where
  toJSON (Action xV yV) = object [
    x .= xV,
    y .= yV ] where
    x = Text.pack("x")
    y = Text.pack("y")
instance FromJSON Action where
  parseJSON (Object v) = Action <$>
                         v .: "action" <*>
                         v .: "value"
  parseJSON _ = empty

main :: IO ()
main = WS.runClient "127.0.0.1" 8081 "/websocket" app

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ handler

    let start = Action "start"
    print start
    WS.sendTextData conn $ encode . start

    WS.sendClose conn

handler :: WS.ClientApp () -> IO ()
handler conn = do
	msg <- WS.receiveData conn
	liftIO $ B.putStrLn msg