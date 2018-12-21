module Day11.Main where

import           Data.Foldable
import           Control.Monad.State
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

main :: IO ()
main = do
  let serialNumber      = 7347
  let areas             = newSummedAreas serialNumber

  let Just (Square a _) = maximumSquareByPowerLevel areas (allSquares 3)
  putStrLn $ "Coordinates of 3x3 square with the most power: " ++ (show a)

  let Just s@(Square b _) =
        maximumSquareByPowerLevel areas ([1 .. 300] >>= allSquares)
  putStrLn $ "Coordinates of the square with the most power: " ++ (show b)
  putStrLn $ "It has size: " ++ (show $ getSize s)


type Coord = (Int, Int)

type FuelCell = Coord

data Square = Square
  { topLeft :: Coord
  , bottomRight :: Coord
  } deriving (Show)

topRight :: Square -> Coord
topRight (Square (_, y) (x', _)) = (x', y)

bottomLeft :: Square -> Coord
bottomLeft (Square (x, _) (_, y')) = (x, y')

type RackId = Int
type SerialNumber = Int

newSummedAreas :: Int -> Map Coord Int
newSummedAreas serialNumber = (`evalState` Map.empty) $ do
  _ <- sumCell (300, 300)
  get
 where
  sumCell :: Coord -> State (Map Coord Int) Int
  sumCell coord@(x, y)
    | x < 1 || y < 1 = pure 0
    | x == 1 || y == 1 = do
      let value = cellPowerLevel serialNumber coord
      modify' (Map.insert coord value)
      pure $ value
    | otherwise = do
      s <- get
      case Map.lookup coord s of
        (Just foundValue) -> pure foundValue
        _                 -> do
          let cellValue = cellPowerLevel serialNumber coord
          topSum     <- sumCell (x, y - 1)
          leftSum    <- sumCell (x - 1, y)
          topLeftSum <- sumCell (x - 1, y - 1)
          let summedValue = cellValue + topSum + leftSum - topLeftSum
          modify' (Map.insert coord summedValue)
          pure summedValue


allCoords :: [Coord]
allCoords = do
  x <- [1 .. 300]
  y <- [1 .. 300]
  pure $ (x, y)

maximumSquareByPowerLevel :: Map Coord Int -> [Square] -> Maybe Square
maximumSquareByPowerLevel summedAreas = foldl' f Nothing
 where
  power = squarePowerLevel summedAreas
  f Nothing  b = Just b
  f (Just b) a = if power b `compare` power a == LT then Just a else Just b

allSquares :: Int -> [Square]
allSquares squareSize = do
  x <- [1 .. 301 - squareSize]
  y <- [1 .. 301 - squareSize]
  pure $ newSquare x y squareSize

newSquare :: Int -> Int -> Int -> Square
newSquare x y squareSize =
  Square (x, y) (x + squareSize - 1, y + squareSize - 1)

getSize :: Square -> Int
getSize (Square (x, _) (x', _)) = x' - x + 1

rackId :: FuelCell -> RackId
rackId (x, _) = x + 10

cellPowerLevel :: Int -> FuelCell -> Int
cellPowerLevel serialNumber cell@(_, y) =
  (subtract 5)
    $ hundredsDigit
    $ (* rackId cell)
    $ (+ serialNumber)
    $ (* y)
    $ rackId cell

squarePowerLevel :: Map Coord Int -> Square -> Maybe Int
squarePowerLevel summedAreas s@(Square (x, y) _) = do
  let s' = s { topLeft = (x - 1, y - 1) }
  a <- Map.lookup (topLeft s') summedAreas
  b <- Map.lookup (topRight s') summedAreas
  c <- Map.lookup (bottomLeft s') summedAreas
  d <- Map.lookup (bottomRight s') summedAreas
  pure $ d - b - c + a

hundredsDigit :: Int -> Int
hundredsDigit a = case reverse (show a) of
  (_ : _ : x : _) -> read [x]
  _               -> 0
