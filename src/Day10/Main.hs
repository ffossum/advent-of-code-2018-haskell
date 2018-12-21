{-# LANGUAGE OverloadedStrings #-}

module Day10.Main where

import           Data.Foldable                  ( traverse_ )
import           Data.List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Attoparsec.Text    hiding ( take )

main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day10/input.txt"
  let (Right points) = traverse parsePoint inputs

  let (finalPoints, secondsTaken) =
        findGatheredPoints $ iterate (fmap updatePosition) points
  let imgPositions = normalize $ getPos <$> finalPoints
  let imgRows      = fmap (fmap fst) $ groupByY $ sortByY imgPositions

  let width        = maximum $ concat imgRows

  let emptyRow     = Map.fromList $ zip [0 .. width] (repeat False)
  let rows =
        fmap (fmap snd)
          $ fmap Map.toList
          $ fmap (`Map.union` emptyRow)
          $ fmap rowImgMap
          $ imgRows

  traverse_ putStrLn $ fmap showRow rows
  putStrLn $ "Waited " ++ (show secondsTaken) ++ " seconds!"

showRow :: [Bool] -> String
showRow = fmap showBool
 where
  showBool True  = '#'
  showBool False = '.'

rowImgMap :: [Int] -> Map Int Bool
rowImgMap xs = foldr Map.union Map.empty $ fmap xToMap xs
  where xToMap x = Map.singleton x True

groupByY :: [Position] -> [[Position]]
groupByY = groupBy (\a b -> snd a == snd b)

sortByY :: [Position] -> [Position]
sortByY = sortBy (\a b -> snd a `compare` snd b)

normalize :: [Position] -> [Position]
normalize ps = fmap (modifyY (subtract minY))
  $ fmap (modifyX (subtract minX)) ps
 where
  minX = minimum (fst <$> ps)
  minY = minimum (snd <$> ps)
  modifyX f (x, y) = (f x, y)
  modifyY f (x, y) = (x, f y)

type Position = (Int, Int)
type Velocity = (Int, Int)
data Point = Point Position Velocity deriving (Show)

type Seconds = Int

findGatheredPoints :: [[Point]] -> ([Point], Seconds)
findGatheredPoints []       = ([], 0)
findGatheredPoints (a : as) = loop (a, pointsWidth a, 0) as
 where
  loop :: ([Point], Int, Seconds) -> [[Point]] -> ([Point], Seconds)
  loop (img, _, s) [] = (img, s)
  loop (img, height, s) (b : bs) =
    let bHeight = pointsWidth b
    in  if (bHeight > height) then (img, s) else loop (b, bHeight, s + 1) bs

updatePosition :: Point -> Point
updatePosition (Point (posX, posY) (velX, velY)) =
  Point (posX + velX, posY + velY) (velX, velY)

getPos :: Point -> Position
getPos (Point pos _) = pos

getPosX :: Point -> Int
getPosX = fst . getPos

pointsWidth :: [Point] -> Int
pointsWidth []  = 0
pointsWidth [_] = 1
pointsWidth (p : ps) =
  let x            = getPosX p
      (minX, maxX) = foldl' minMaxX (x, x) ps
  in  maxX - minX
 where
  minMaxX :: (Int, Int) -> Point -> (Int, Int)
  minMaxX (minX, maxX) (Point (x, _) _) = (min minX x, max maxX x)

parsePoint :: Text -> Either String Point
parsePoint = parseOnly $ do
  _    <- string "position=<" <* skipSpace
  posX <- signed decimal
  _    <- char ',' <* skipSpace
  posY <- signed decimal
  _    <- string "> velocity=<" <* skipSpace
  velX <- signed decimal
  _    <- char ',' <* skipSpace
  velY <- signed decimal
  pure $ Point (posX, posY) (velX, velY)
