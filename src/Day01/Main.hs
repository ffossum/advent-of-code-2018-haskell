module Day01.Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Attoparsec.Text

main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day01/input.txt"
  let (Right frequencies) = traverse parseFrequency inputs

  let totalSum            = sum frequencies
  putStrLn $ "Total sum: " <> (show totalSum)

  let (Just firstRepeated) =
        findFirstRepeated (scanl (+) 0 (cycle frequencies))
  putStrLn $ "First sum reached twice: " <> (show firstRepeated)

parseFrequency :: Text -> Either String Int
parseFrequency = parseOnly frequency where frequency = signed decimal

findFirstRepeated :: [Int] -> Maybe Int
findFirstRepeated as = loop Set.empty as
 where
  loop :: Set Int -> [Int] -> Maybe Int
  loop _ [] = Nothing
  loop seen (b : bs) =
    if (Set.member b seen) then Just b else loop (Set.insert b seen) bs
