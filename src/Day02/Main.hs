{-# LANGUAGE OverloadedStrings #-}

module Day02.Main where
import           Data.List                      ( foldl'
                                                , subsequences
                                                , nub
                                                )
import           Data.Functor
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

main :: IO ()
main = do
  inputs <- lines <$> readFile "src/Day02/input.txt"
  let allLetterCounts = letterCounts <$> inputs
  let idsWithExactly2 = length $ filter (hasAnyExactlyNTimes 2) allLetterCounts
  let idsWithExactly3 = length $ filter (hasAnyExactlyNTimes 3) allLetterCounts
  let checksum        = idsWithExactly2 * idsWithExactly3
  putStrLn $ "Checksum: " <> (show checksum)

  let allWithOneCharacterRemoved = inputs >>= oneCharacterRemoved
  let (Just firstMatch) = findFirstRepeated allWithOneCharacterRemoved
  putStrLn $ "Common characters in closest IDs: " <> (show firstMatch)

letterCounts :: String -> Map Char Int
letterCounts = foldl' f Map.empty
 where
  f counts letter = Map.alter g letter counts
  g (Just prev) = Just (prev + 1)
  g Nothing     = Just 1

hasAnyExactlyNTimes :: Int -> Map Char Int -> Bool
hasAnyExactlyNTimes n = Map.foldr (||) False . Map.map (== n)

oneCharacterRemoved :: String -> [String]
oneCharacterRemoved str = nub $ (uncurry deleteN) <$> zip [0 ..] strs
 where
  len  = (length str) - 1
  strs = take len (repeat str)

deleteN :: Int -> [a] -> [a]
deleteN _ [] = []
deleteN i (a : as) | i == 0    = as
                   | otherwise = a : deleteN (i - 1) as

findFirstRepeated :: Ord a => [a] -> Maybe a
findFirstRepeated = loop Set.empty
  where
    loop _ [] = Nothing
    loop seen (a : as) =
      if (Set.member a seen) then Just a else loop (Set.insert a seen) as
