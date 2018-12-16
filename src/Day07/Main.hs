{-# LANGUAGE OverloadedStrings #-}

module Day07.Main where

import           Data.Char                      ( ord )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Attoparsec.Text
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Step = Char
data Dependency = Step `DependsOn` Step deriving (Show)
type DependencyMap = Map Step (Set Step)

main :: IO ()
main = do
  inputs <- Text.lines <$> TextIO.readFile "src/Day07/input.txt"
  let (Right dependencies) = traverse parseDependency inputs
  let dependencyMap =
        foldr (Map.unionWith Set.union)
              Map.empty
              (dependencyToMap <$> dependencies)
          `Map.union` Map.fromSet (const Set.empty) (getAllSteps dependencies)

  let stepOrder = solveStepOrder dependencyMap []

  putStrLn $ "The correct step order is: " ++ stepOrder

  let timeSpent = solveStepsWithTiming dependencyMap Map.empty 0

  putStrLn
    $  "The workers spent "
    ++ (show timeSpent)
    ++ " seconds to complete all the steps."


solveStepOrder :: DependencyMap -> [Step] -> [Step]
solveStepOrder dependencies stepsDone
  | Map.null dependencies
  = reverse stepsDone
  | otherwise
  = let nextStep = fst $ Map.findMin $ Map.filter null dependencies
        nextDependencies =
          (Set.delete nextStep) <$> Map.delete nextStep dependencies
    in  solveStepOrder nextDependencies (nextStep : stepsDone)

getAllSteps :: [Dependency] -> Set Step
getAllSteps ds = Set.fromList as `Set.union` Set.fromList bs
 where
  as = (\(a `DependsOn` _) -> a) <$> ds
  bs = (\(_ `DependsOn` b) -> b) <$> ds

parseDependency :: Text -> Either String Dependency
parseDependency = parseOnly $ do
  _ <- string "Step "
  a <- anyChar
  _ <- string " must be finished before step "
  b <- anyChar
  pure $ b `DependsOn` a

dependencyToMap :: Dependency -> DependencyMap
dependencyToMap (a `DependsOn` b) = Map.singleton a (Set.singleton b)

type Workers = Map Step Int

solveStepsWithTiming :: DependencyMap -> Workers -> Int -> Int
solveStepsWithTiming dependencies workers timeSpent =
  let finishedSteps   = Map.keys $ Map.filter (== 0) workers
      newDependencies = removeSteps finishedSteps
        $ foldr Map.delete dependencies finishedSteps
      newWorkers =
        addNewWorkers newDependencies $ foldr Map.delete workers finishedSteps
      timeToSpend = Map.foldl' min 1 newWorkers
  in  if Map.null newDependencies
        then timeSpent
        else solveStepsWithTiming newDependencies
                                  (fmap (subtract timeToSpend) newWorkers)
                                  (timeSpent + timeToSpend)

removeSteps :: [Step] -> DependencyMap -> DependencyMap
removeSteps steps deps = foldr (fmap . Set.delete) deps steps

addNewWorkers :: DependencyMap -> Workers -> Workers
addNewWorkers dependencies oldWorkers
  | Map.size oldWorkers >= 5 || Set.null availableSteps
  = oldWorkers
  | otherwise
  = let nextStep    = Set.findMin availableSteps
        nextWorkers = Map.insert nextStep (getTimeRequired nextStep) oldWorkers
    in  addNewWorkers dependencies nextWorkers
 where
  availableSteps :: Set Step
  availableSteps =
    Set.filter (`Map.notMember` oldWorkers) $ Map.keysSet $ Map.filter
      null
      dependencies

getTimeRequired :: Step -> Int
getTimeRequired s = 61 + (ord s) - (ord 'A')
