{-# LANGUAGE OverloadedStrings #-}

module Day04.Main where

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Attoparsec.Text          as Parse
import           Data.Attoparsec.Text

type Minute = Int
type GuardId = Int
data LogEntry = BeginShift GuardId | FallAsleep Minute | WakeUp Minute deriving (Eq, Show)

inSameShift :: LogEntry -> LogEntry -> Bool
inSameShift _ (BeginShift _) = False
inSameShift _ _              = True

data Shift = Shift
  { shiftGuardId :: GuardId
  , shiftNaps :: [Nap]
  } deriving (Eq, Show)

data Nap = Nap
  { napStart :: Minute
  , napEnd :: Minute
  } deriving (Eq, Show)

shiftListToMap :: [Shift] -> Map GuardId [Nap]
shiftListToMap as = foldl' (Map.unionWith (++)) Map.empty (shiftToMap <$> as)
 where
  shiftToMap :: Shift -> Map GuardId [Nap]
  shiftToMap (Shift gid ns) = Map.singleton gid ns

napLength :: Nap -> Int
napLength (Nap t0 t1) = t1 - t0

minuteCounts :: Nap -> Map Minute Int
minuteCounts (Nap t0 t1) = Map.fromList (zip [t0 .. t1 - 1] [1, 1 ..])

entriesToShift :: [LogEntry] -> Maybe Shift
entriesToShift ((BeginShift gid) : ls) = (Shift gid) <$> (entriesToNaps ls)
 where
  entriesToNaps :: [LogEntry] -> Maybe [Nap]
  entriesToNaps [] = Just []
  entriesToNaps ((FallAsleep m1) : (WakeUp m2) : ls') =
    ((Nap m1 m2) :) <$> entriesToNaps ls'
  entriesToNaps _ = Nothing
entriesToShift _ = Nothing

parseLogEntry :: Text -> Either String LogEntry
parseLogEntry = parseOnly $ do
  _        <- Parse.take 15
  m        <- decimal
  _        <- Parse.take 2
  logEntry <- choice
    [ BeginShift <$> (string "Guard #" *> decimal <* " begins shift")
    , (FallAsleep m) <$ string "falls asleep"
    , (WakeUp m) <$ string "wakes up"
    ]
  pure logEntry

maxWithKey :: (Minute, Int) -> Minute -> Int -> (Minute, Int)
maxWithKey (k0, b) k a = if (a > b) then (k, a) else (k0, b)

getMaximumValue :: (Ord a, Ord b) => Map a b -> Maybe (a, b)
getMaximumValue m = if Map.null m
  then Nothing
  else Just $ maximumBy compareMapValues (Map.toList m)

compareMapValues :: Ord b => (a, b) -> (a, b) -> Ordering
compareMapValues e1 e2 = (snd e1) `compare` (snd e2)

getSleepiestMinute :: [Nap] -> Maybe (Minute, Int)
getSleepiestMinute naps =
  let counts = foldl' (Map.unionWith (+)) Map.empty (minuteCounts <$> naps)
  in  getMaximumValue counts

main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day04/input.txt"
  let (Right entries)             = traverse parseLogEntry (sort inputs)
  let (Just shifts) = traverse entriesToShift (groupBy inSameShift entries)
  let guardToNaps                 = shiftListToMap shifts

  let totalNapLength              = Map.map (sum . (map napLength)) guardToNaps
  let (Just (sleepiestGuard, _))  = getMaximumValue totalNapLength
  let (Just sleepiestNaps)        = Map.lookup sleepiestGuard guardToNaps
  let (Just (sleepiestMinute, _)) = getSleepiestMinute sleepiestNaps

  putStrLn $ "Guard #" ++ (show sleepiestGuard) ++ " was the sleepiest."
  putStrLn $ "He slept the most at minute " ++ (show sleepiestMinute) ++ "."
  putStrLn $ "Answer: " ++ (show $ sleepiestGuard * sleepiestMinute)

  let (Just guardToMinuteCounts) =
        sequence $ Map.filter isJust (getSleepiestMinute <$> guardToNaps)
  let guardToHighestMinuteCount = snd <$> guardToMinuteCounts
  let (Just (guardWithHighest, highestMinuteCount)) =
        getMaximumValue guardToHighestMinuteCount
  let (Just minuteWithHighest) =
        fst <$> Map.lookup guardWithHighest guardToMinuteCounts

  putStrLn
    $  "Guard #"
    ++ (show guardWithHighest)
    ++ " was most frequently asleep on the same minute."
  putStrLn
    $  "He was asleep "
    ++ (show highestMinuteCount)
    ++ " times on minute "
    ++ (show minuteWithHighest)
    ++ "."
  putStrLn
    $  "Day 4, part 2 answer: "
    ++ (show $ guardWithHighest * minuteWithHighest)
