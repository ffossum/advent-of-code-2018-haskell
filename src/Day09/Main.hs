{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedLists #-}

module Day09.Main where

import           Data.Maybe
import           Data.List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq


type Marble = Int

data Circle = Circle
  { circleMarbles :: Seq Marble
  , circleCurrent :: Int
  } deriving (Show)

type PlayerId = Int
type Score = Int

type Players = Map PlayerId Score

circleSize :: Circle -> Int
circleSize = Seq.length . circleMarbles

newPlayers :: Int -> Players
newPlayers numberOfPlayers =
  Map.fromList $ zip [0 .. numberOfPlayers - 1] [0, 0 ..]

addMarble :: Marble -> Circle -> Circle
addMarble marble circle = Circle
  (Seq.insertAt idx marble (circleMarbles circle))
  idx
  where idx = (circleCurrent circle + 2) `mod` circleSize circle

removeMarble :: Circle -> (Marble, Circle)
removeMarble circle = (marble, nextCircle)
 where
  idx = (circleCurrent circle + circleSize circle - 7) `mod` circleSize circle
  marble = circleMarbles circle `Seq.index` idx
  nextCircle = circle { circleMarbles = Seq.deleteAt idx (circleMarbles circle)
                      , circleCurrent = idx
                      }

newCircle :: Circle
newCircle = Circle (Seq.singleton 0) 0

performTurn
  :: Int -> Marble -> (Players, PlayerId, Circle) -> (Players, PlayerId, Circle)
performTurn noOfPlayers marble (players, currentPlayer, circle)
  | currentPlayer >= noOfPlayers
  = performTurn noOfPlayers marble (players, 0, circle)
  | marble `mod` 23 == 0
  = let (removed, nextCircle) = removeMarble circle
        nextPlayers = Map.adjust (+ (marble + removed)) currentPlayer players
    in  (nextPlayers, currentPlayer + 1, nextCircle)
  | otherwise
  = (players, currentPlayer + 1, addMarble marble circle)

main :: IO ()
main = do
  let numberOfPlayers = 493
      lastMarble      = 71863

  let (players, _, _) = foldl' (flip (performTurn numberOfPlayers))
                               (newPlayers numberOfPlayers, 0, newCircle)
                               [1 .. lastMarble]

  let maxScore = Map.foldl' max 0 players

  putStrLn $ "The winning score is: " ++ (show maxScore)


