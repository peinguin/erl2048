{-# LANGUAGE DeriveDataTypeable #-}
module Types (
  Action(..),
  Cell(..),
  Game(..),
  PreviousPosition(..),
  getBest
) where

import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Typeable as Typeable

data Action = Action String (Maybe String) deriving (Show)
data Cell = Cell Int (Maybe [Cell]) (Maybe PreviousPosition) deriving (Typeable.Typeable, Show)
data PreviousPosition = PreviousPosition Int Int deriving (Typeable.Typeable, Show)
{-
data Score = Score { name :: String, score :: Int } deriving (Show)
data User = User {
  id       :: Maybe Int,
  username :: Maybe String
} deriving (Show)
-}

data Game = Game {
--  player      :: User,
  currScore   :: Int,
--  scores      :: [Score],
--  won         :: Bool,
--  over        :: Bool,
--  keepPlaying :: Bool,
  grid        :: [[Maybe Cell]]
} deriving (Show)

instance Aeson.ToJSON Action where
  toJSON (Action actionV valueV) = Aeson.object [
	action Aeson..= actionV,
	value Aeson..= valueV ] where
	action = Text.pack("action")
	value = Text.pack("value")
instance Aeson.FromJSON Game where
	parseJSON (Aeson.Object v) = Game <$> 
--                                     v Aeson..: Text.pack("user") <*>
                                     v Aeson..: Text.pack("score") <*>
--                                     v Aeson..: Text.pack("scores") <*>
--                                     v Aeson..: Text.pack("won") <*>
--                                     v Aeson..: Text.pack("over") <*>
--                                     v Aeson..: Text.pack("keepPlaying") <*>
                                     v Aeson..: Text.pack("grid")
	parseJSON _ = empty
instance Aeson.FromJSON Cell where
	parseJSON (Aeson.Object v) = Cell <$>
                                     v Aeson..: Text.pack("value") <*>
                                     v Aeson..: Text.pack("mergedFrom") <*>
                                     v Aeson..: Text.pack("previousPosition")
	parseJSON _ = empty
{-
instance Aeson.FromJSON User where
	parseJSON (Aeson.Object v) = User <$>
                                     v Aeson..: Text.pack("id") <*>
                                     v Aeson..: Text.pack("name")
	parseJSON _ = empty
instance Aeson.FromJSON Score where
	parseJSON (Aeson.Object v) = Score <$>
                                     v Aeson..: Text.pack("name") <*>
                                     v Aeson..: Text.pack("score")
	parseJSON _ = empty
-}
instance Aeson.FromJSON PreviousPosition where
	parseJSON (Aeson.Object v) = PreviousPosition <$>
                                     v Aeson..: Text.pack("x") <*>
                                     v Aeson..: Text.pack("y")
	parseJSON _ = empty
        
getBest :: [[Maybe Cell]] -> Int
getBest [] = 0
getBest [x] =
  getBestFromRow(x)
getBest (x:xs)   
    | bestFromRow >= bestFromTail = bestFromRow
    | otherwise = bestFromTail
    where
      bestFromRow = getBestFromRow(x)
      bestFromTail = getBest(xs)

getBestFromRow :: [Maybe Cell] -> Int
getBestFromRow [] = 0
getBestFromRow [Nothing] = 0
getBestFromRow [Just (Cell x _ _)] = x
getBestFromRow (Nothing:xs) = getBestFromRow(xs)
getBestFromRow (Just (Cell x _ _):xs)
  | x >= bestFromTail = x
  | otherwise = bestFromTail
    where
      bestFromTail = getBestFromRow(xs)
