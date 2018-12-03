{-# LANGUAGE OverloadedStrings #-}

module Day03.Main where

import           Data.List                      ( foldl'
                                                , find
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Attoparsec.Text           ( parseOnly
                                                , string
                                                , char
                                                , decimal
                                                )

newtype ClaimId = ClaimId Int deriving (Eq, Ord, Show)
data Claim = Claim
  { claimId :: ClaimId
  , xMin :: Int
  , yMin :: Int
  , xSize :: Int
  , ySize :: Int
} deriving (Eq, Show)

type Coord = (Int, Int)

coords :: Claim -> [Coord]
coords c = do
  x <- [xMin c .. xMin c + xSize c - 1]
  y <- [yMin c .. yMin c + ySize c - 1]
  pure (x, y)

coordCounts :: Claim -> Map Coord Int
coordCounts = Map.fromList . (`zip` [1, 1 ..]) . coords

letterCounts :: String -> Map Char Int
letterCounts = foldl' f Map.empty
 where
  f counts letter = Map.alter g letter counts
  g (Just prev) = Just (prev + 1)
  g Nothing     = Just 1

parseClaim :: Text -> Either String Claim
parseClaim = parseOnly $ do
  cid              <- parseClaimId
  _                <- string " @ "
  (xMin', yMin')   <- parseCoords
  _                <- string ": "
  (xSize', ySize') <- parseSizes
  pure $ Claim cid xMin' yMin' xSize' ySize'
 where
  parseClaimId = ClaimId <$> (char '#' *> decimal)
  parseCoords  = do
    x <- decimal
    _ <- char ','
    y <- decimal
    pure (x, y)
  parseSizes = do
    x <- decimal
    _ <- char 'x'
    y <- decimal
    pure (x, y)


main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day03/input.txt"
  let (Right claims) = traverse parseClaim inputs
  let allCoordCounts = coordCounts <$> claims
  let combinedCounts = foldl' (Map.unionWith (+)) Map.empty allCoordCounts
  let overlapped     = Map.size $ Map.filter (> 1) combinedCounts
  putStrLn
    $  "Total square inches covered by more than 1 claim: "
    <> (show overlapped)

  let (Just notOverlapping) = find (hasNoOverlaps combinedCounts) claims
  let (ClaimId cid)         = claimId notOverlapping
  putStrLn $ "ID# of claim not overlapping: " <> (show cid)


hasNoOverlaps :: Map Coord Int -> Claim -> Bool
hasNoOverlaps counts claim = all f (coords claim)
 where
  f coord = case Map.lookup coord counts of
    (Just claimCount) -> claimCount == 1
    Nothing           -> True
