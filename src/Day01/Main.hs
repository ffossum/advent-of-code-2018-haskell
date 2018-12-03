module Day01.Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import qualified Data.IntSet                   as IntSet
import           Data.Attoparsec.Text

main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day01/input.txt"
  let (Right frequencies) = traverse parseFrequency inputs

  let totalSum            = sum frequencies
  putStrLn $ "Total sum: " <> (show totalSum)

  let (Just firstRepeated) =
        findFirstRepeated $ scanl (+) 0 (cycle frequencies)
  putStrLn $ "First sum reached twice: " <> (show firstRepeated)

parseFrequency :: Text -> Either String Int
parseFrequency = parseOnly $ signed decimal

findFirstRepeated :: [Int] -> Maybe Int
findFirstRepeated = loop IntSet.empty
 where
  loop _ [] = Nothing
  loop seen (a : as) =
    if (IntSet.member a seen) then Just a else loop (IntSet.insert a seen) as
