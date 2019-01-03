{-# LANGUAGE OverloadedStrings #-}

module Day14.Main where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

main :: IO ()
main = do
  let n                         = 165061
  let (Kitchen _ finalRecipes1) = applyNTimes next testKitchen (n + 10)
  let answer1 =
        fmap intToDigit $ toList $ fmap recipeScore $ Seq.take 10 $ Seq.drop
          n
          finalRecipes1

  putStrLn $ "The first ten recipes after N recipes: " ++ answer1

  let needle = "165061"
  let (Kitchen _ finalRecipes2) = applyNTimesUntil
        next
        testKitchen
        100000
        (recipesContain needle . recipes)

  putStrLn $ "Digits appearing before: " ++ show
    (recipesBefore needle finalRecipes2)

recipesBefore :: Text -> Seq Recipe -> Int
recipesBefore t rs =
  let (b, _) = Text.breakOn t (recipesToText rs) in Text.length b

recipesContain :: Text -> Seq Recipe -> Bool
recipesContain t rs = case Text.breakOn t (recipesToText rs) of
  (_, "") -> False
  (_, _ ) -> True

recipesToText :: Seq Recipe -> Text
recipesToText = Text.pack . fmap intToDigit . fmap recipeScore . toList

newtype Recipe = Recipe Int deriving (Eq, Show)

recipeScore :: Recipe -> Int
recipeScore (Recipe x) = x

data Kitchen = Kitchen
  { indices :: [Int]
  , recipes :: Seq Recipe
  } deriving (Eq, Show)

next :: Kitchen -> Kitchen
next k =
  let rs' = nextRecipes k
      is' = fmap (nextIndex rs') (indices k)
  in  Kitchen is' rs'

nextRecipes :: Kitchen -> Seq Recipe
nextRecipes (Kitchen is rs) =
  let currentRecipes = fmap (Seq.index rs) is
      totalScore     = sum $ fmap recipeScore $ currentRecipes
      newRecipes     = Seq.fromList $ fmap Recipe $ digits totalScore
  in  rs <> newRecipes

nextIndex :: Seq Recipe -> Int -> Int
nextIndex rs i =
  let size = Seq.length rs
      r    = rs `Seq.index` i
      i'   = (i + recipeScore r + 1) `mod` size
  in  i'

applyNTimes :: (a -> a) -> a -> Int -> a
applyNTimes f a n | n > 0     = applyNTimes f (f a) (n - 1)
                  | otherwise = a

applyNTimesUntil :: (a -> a) -> a -> Int -> (a -> Bool) -> a
applyNTimesUntil f a n p
  | p a       = a
  | otherwise = applyNTimesUntil f (applyNTimes f a n) n p

testKitchen = Kitchen [0, 1] (Seq.fromList [Recipe 3, Recipe 7])

digits :: Int -> [Int]
digits = map digitToInt . show . abs
