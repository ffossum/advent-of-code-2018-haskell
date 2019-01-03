module Day14.Main where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.List                      ( isPrefixOf )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq

main :: IO ()
main = do
  let n = 165061
  let answer1 =
        fmap intToDigit $ fmap recipeScore $ (take 10 $ drop n $ allRecipes)

  putStrLn $ "The first ten recipes after N recipes: " ++ answer1

  let pattern        = fmap Recipe $ fmap digitToInt $ "165061"
  let (Just answer2) = indexOfPattern pattern allRecipes

  putStrLn $ "Number of digits appearing before the pattern: " ++ show answer2

type Index = Int
newtype Recipe = Recipe Int deriving (Eq, Show)

allRecipes :: [Recipe]
allRecipes = Recipe 3 : Recipe 7 : go [0, 1]
                                      (Seq.fromList [Recipe 3, Recipe 7])
 where
  go :: [Index] -> Seq Recipe -> [Recipe]
  go is rs =
    let newRecipes = createNewRecipes is rs
        rs'        = rs <> Seq.fromList newRecipes
        is'        = fmap (nextIndex rs') is
    in  newRecipes ++ go is' rs'

indexOfPattern :: Eq a => [a] -> [a] -> Maybe Index
indexOfPattern pattern = go 0
 where
  go _ [] = Nothing
  go idx as | pattern `isPrefixOf` as = Just idx
            | otherwise               = go (idx + 1) (tail as)

recipeScore :: Recipe -> Int
recipeScore (Recipe x) = x

createNewRecipes :: [Index] -> Seq Recipe -> [Recipe]
createNewRecipes is rs =
  let currentRecipes = fmap (Seq.index rs) is
      totalScore     = sum $ fmap recipeScore $ currentRecipes
  in  fmap Recipe $ digits totalScore

nextIndex :: Seq Recipe -> Int -> Int
nextIndex rs i =
  let size = Seq.length rs
      r    = rs `Seq.index` i
      i'   = (i + recipeScore r + 1) `mod` size
  in  i'

digits :: Int -> [Int]
digits = map digitToInt . show . abs
