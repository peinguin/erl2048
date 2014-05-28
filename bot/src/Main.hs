module Main ( main ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad.Trans (liftIO)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO(putStrLn)
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

import qualified Data.HashMap.Strict as H

data Action = Action {
  action :: String,
  value :: Maybe String
}deriving (Show)

data PreviousPosition = Nothing | PreviousPosition { x :: Integer, y :: Integer}
data Cell = Cell { value :: Integer, mergedFrom :: [Cell], previousPosition :: PreviousPosition }
data Score = Score { name :: String, score :: Integer }
data User = User { id :: Integer, user :: String }

data Game = Game {
  grid        :: [[Cell]],
  user        :: User,
  score       :: Integer,
  scores      :: [Score],
  won         :: Bool,
  over        :: Bool,
  keepPlaying :: Bool
} deriving (Show)

instance Aeson.ToJSON Action where
  toJSON (Action actionV valueV) = Aeson.object [
	action Aeson..= actionV,
	value Aeson..= valueV ] where
	action = Text.pack("action")
	value = Text.pack("value")
instance Aeson.FromJSON Game where
	parseJSON (Aeson.Object v) = Game <$>
                                     v Aeson..: Text.pack("grid") <*>
                                     v Aeson..: Text.pack("user") <*>
                                     v Aeson..: Text.pack("score") <*>
                                     v Aeson..: Text.pack("scores") <*>
                                     v Aeson..: Text.pack("won") <*>
                                     v Aeson..: Text.pack("over") <*>
                                     v Aeson..: Text.pack("keepPlaying")
	parseJSON _ = empty

main :: IO ()
main = WS.runClient "127.0.0.1" 8081 "/websocket" app

app :: WS.ClientApp ()
app conn = do
	WS.sendTextData conn (Aeson.encode Action {action = "start", value = Nothing })
        
        loop conn
        
	WS.sendClose conn ( Text.pack "Bye!" )

loop :: WS.ClientApp ()
loop conn =
  Aeson.decode <$> WS.receiveData conn >>=
  print
-- >>
--  loop
--   let decoded = Aeson.decode =<< 
--   in print decoded
   --decoded <- liftIO $ Aeson.decode msg
   --print decode
--   loop