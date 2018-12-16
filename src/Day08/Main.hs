{-# LANGUAGE OverloadedStrings #-}

module Day08.Main where

import           Data.Maybe
import           Data.List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Attoparsec.Text
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

data Node = Node [Node] [Int] deriving (Eq, Show)

testInput :: Text
testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

main :: IO ()
main = do
  [input] <- Text.lines <$> TextIO.readFile "src/Day08/input.txt"
  let (Right node) = parseNode input

  putStrLn $ "The sum of all metadata is: " ++ (show $ sumMetadata node)
  putStrLn $ "The node value of the root node is: " ++ (show $ nodeValue node)

parseNode :: Text -> Either String Node
parseNode = parseOnly $ node
 where
  node :: Parser Node
  node = do
    noOfChildren <- decimal <* skipSpace
    noOfMeta     <- decimal <* skipSpace
    children     <- count noOfChildren node
    meta         <- count noOfMeta (decimal <* skipSpace)
    pure $ Node children meta

sumMetadata :: Node -> Int
sumMetadata (Node children meta) =
  foldl' (\b a -> b + sumMetadata a) 0 children + sum meta

nodeValue :: Node -> Int
nodeValue (Node []       meta) = sum meta
nodeValue (Node children meta) = sum $ do
  m     <- meta
  child <- maybeToList $ lookup m indexedChildren
  pure $ nodeValue child
  where indexedChildren = [1 ..] `zip` children


