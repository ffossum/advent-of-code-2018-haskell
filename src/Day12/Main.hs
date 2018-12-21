{-# LANGUAGE OverloadedStrings #-}

module Day12.Main where

import           Control.Monad
import           Data.Functor
import           Data.Maybe
import           Data.List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Attoparsec.Text    hiding ( takeWhile
                                                , take
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

main :: IO ()
main = do
  input <- TextIO.readFile "src/Day12/input.txt"
  let (Right (pots, funcs)) = parsePots input

  putStrLn
    $  "Sum of pot numbers with plants after 20 generations: "
    ++ (show $ sum $ indicesWithPots $ getGenerationN funcs 20 pots)

  putStrLn
    $  "Sum of pot numbers with plants after 500 generations: "
    ++ (show $ sum $ indicesWithPots $ getGenerationN funcs 500 pots)

  putStrLn
    $  "Sum of pot numbers with plants after 5000 generations: "
    ++ (show $ sum $ indicesWithPots $ getGenerationN funcs 5000 pots)

  putStrLn
    $  "Sum of pot numbers with plants after 50000 generations: "
    ++ (show $ sum $ indicesWithPots $ getGenerationN funcs 50000 pots)

getGenerationN :: [SpreadFunc] -> Int -> Pots -> Pots
getGenerationN funcs n = loop 0
 where
  loop :: Int -> Pots -> Pots
  loop gen prev | gen == n  = prev
                | otherwise = loop (gen + 1) (nextGeneration funcs prev)

indicesWithPots :: Pots -> [Int]
indicesWithPots (Pots idx pots) = map fst $ filter snd $ zip [idx ..] pots

nextGeneration :: [SpreadFunc] -> Pots -> Pots
nextGeneration funcs previous =
  let pots = padPots previous
      potFives =
        overlappingFives ([False, False] ++ asList pots ++ [False, False])
      spreadResults = checkSpread funcs <$> potFives
      newPots       = zipWith f spreadResults (asList pots)
  in  trimPots $ pots { asList = newPots }
 where
  f (Just b) _ = b
  f _        b = b


checkSpread :: [SpreadFunc] -> [Bool] -> Maybe Bool
checkSpread funcs b = join $ find isJust $ fmap ($ b) funcs

padPots :: Pots -> Pots
padPots (Pots index pots) = Pots (index - 4) (fourEmpty ++ pots ++ fourEmpty)
  where fourEmpty = [False, False, False, False]

trimPots :: Pots -> Pots
trimPots (Pots index pots) =
  let droppedLeft = length $ takeWhile (== False) pots
      newPots     = drop droppedLeft $ dropWhileEnd (== False) $ pots
  in  Pots (index + droppedLeft) newPots

overlappingFives :: [a] -> [[a]]
overlappingFives (a1 : a2 : a3 : a4 : a5 : as) =
  [a1, a2, a3, a4, a5] : overlappingFives (a2 : a3 : a4 : a5 : as)
overlappingFives _ = []

data Pots = Pots
  { startIndex :: Int
  , asList :: [Bool]
  } deriving (Show)

type SpreadFunc = [Bool] -> Maybe Bool

newSpreadFunc :: [Bool] -> Bool -> SpreadFunc
newSpreadFunc as result = f
 where
  f bs | as == bs  = Just result
       | otherwise = Nothing

parsePots :: Text -> Either String (Pots, [SpreadFunc])
parsePots = parseOnly $ do
  _     <- string "initial state: "
  pots  <- many1' pot
  _     <- count 2 endOfLine
  funcs <- sepBy1' spreadFunc endOfLine
  pure $ (Pots 0 pots, funcs)
 where
  pot :: Parser Bool
  pot = choice [char '.' $> False, char '#' $> True]
  spreadFunc :: Parser SpreadFunc
  spreadFunc = do
    bs  <- many1' pot
    _   <- string " => "
    res <- pot
    pure $ newSpreadFunc bs res


