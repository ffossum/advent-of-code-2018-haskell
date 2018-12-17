{-# LANGUAGE OverloadedStrings #-}

module Day06.Main where
import           Data.List                      ( sort
                                                , group
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Attoparsec.Text
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day06/input.txt"
  let (Right coords) = traverse parseCoord inputs

  let xCoords        = fst <$> coords
  let yCoords        = snd <$> coords

  let xMin = minimum xCoords
      xMax = maximum xCoords
      yMin = minimum yCoords
      yMax = maximum yCoords

  let topLeft     = (xMin, yMin)
      bottomRight = (xMax, yMax)


  let boundingLocations = getBoundaryLocations topLeft bottomRight
      insideLocations   = getInsideLocations topLeft bottomRight

  let infiniteAreaCoords =
        Set.foldl' Set.union Set.empty
          $ Set.filter ((== 1) . Set.size)
          $ Set.map (closestCoords coords) boundingLocations

  let finiteAreaCoords =
        Set.difference (Set.fromList coords) infiniteAreaCoords

  let largestArea =
        maximum
          $ fmap length
          $ group
          $ sort
          $ fmap snd
          $ Map.toList
          $ Map.filter (`Set.member` finiteAreaCoords)
          $ Map.map (head . Set.toList)
          $ Map.filter ((== 1) . Set.size)
          $ Map.map (closestCoords coords) (Map.fromSet id insideLocations)

  putStrLn $ "The largest finite area is " ++ (show largestArea)

  let regionSize =
        Map.size
          $ Map.filter (< 10000)
          $ fmap (totalDistance coords)
          $ Map.fromSet id insideLocations

  putStrLn $ "The size of the region is " ++ (show regionSize)


getBoundaryLocations :: Location -> Location -> Set Location
getBoundaryLocations (x1, y1) (x2, y2)
  | x1 > x2   = Set.empty
  | y1 > y2   = Set.empty
  | otherwise = Set.unions [topRow, bottomRow, leftColumn, rightColumn]
 where
  xs          = [x1 .. x2]
  ys          = [y1 .. y2]
  topRow      = Set.fromList $ (,) <$> xs <*> [y1]
  bottomRow   = Set.fromList $ (,) <$> xs <*> [y2]
  leftColumn  = Set.fromList $ (,) <$> [x1] <*> ys
  rightColumn = Set.fromList $ (,) <$> [x2] <*> ys

getInsideLocations :: Location -> Location -> Set Location
getInsideLocations (x1, y1) (x2, y2)
  | x1 > x2 = Set.empty
  | y1 > y2 = Set.empty
  | otherwise = Set.fromList $ do
    x <- [x1 + 1 .. x2 - 1]
    y <- [y1 + 1 .. y2 - 1]
    pure $ (x, y)


type Distance = Int

closestCoords :: [Coord] -> Location -> Set Coord
closestCoords allCoords = snd . Map.findMin . distances allCoords

distances :: [Coord] -> Location -> Map Distance (Set Coord)
distances allCoords loc = Map.fromListWith Set.union $ do
  c <- allCoords
  let d = distance loc c
  pure $ (d, Set.singleton c)

distance :: Coord -> Coord -> Distance
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

totalDistance :: [Coord] -> Location -> Distance
totalDistance allCoords loc = sum (distance loc <$> allCoords)

type Coord = (Int, Int)
type Location = Coord

parseCoord :: Text -> Either String Coord
parseCoord = parseOnly $ do
  x <- decimal
  _ <- string ", "
  y <- decimal
  pure $ (x, y)
