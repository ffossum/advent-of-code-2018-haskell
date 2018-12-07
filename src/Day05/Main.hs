{-# LANGUAGE OverloadedStrings #-}

module Day05.Main where

import           Data.Char
import qualified Data.Map.Strict               as Map

main :: IO ()
main = do
  [input] <- lines <$> readFile "src/Day05/input.txt"
  let stable         = removeReactionsUntilStable input
  let remainingUnits = length stable
  putStrLn $ "Remaining units after fully reacting: " ++ (show remainingUnits)

  let chars = ['a' .. 'z']
  let variations = Map.mapWithKey (\k v -> filter (not . isSameType k) v)
        $ Map.fromList (zip chars (repeat input))
  let reduced        = removeReactionsUntilStable <$> variations
  let shortestLength = minimum $ length <$> reduced
  putStrLn $ "Shortest possible polymer length: " ++ (show shortestLength)


isSameType :: Char -> Char -> Bool
isSameType a b = a == b || toUpper a == toUpper b

isReacting :: Char -> Char -> Bool
isReacting a b = a /= b && toUpper a == toUpper b

removeReactionsUntilStable :: String -> String
removeReactionsUntilStable = foldr step ""
 where
  step a (b : bs) | isReacting a b = bs
  step a bs                        = a : bs
