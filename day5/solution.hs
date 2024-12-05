{-
Advent of Code 2024 Day 5
Adam Gluck
-}

import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

parseRulesAndUpdate :: String -> ([String], [String])
parseRulesAndUpdate input =
  let inputLines = filter (not . null) $ lines input
      (rules, updates) = partition (elem '|') inputLines
   in (rules, updates)

parseRule :: String -> (Int, Int)
parseRule rule = (read first, read second)
  where
    (first, _ : second) = break (== '|') rule

buildRuleMap :: [String] -> Map Int [Int]
buildRuleMap = foldl addToMap Map.empty . map parseRule
  where
    addToMap acc (before, after) = Map.insertWith (++) before [after] acc

parseUpdate :: String -> [Int]
parseUpdate = map read . words . map (\c -> if c == ',' then ' ' else c)

isUpdateValid :: Map Int [Int] -> String -> Bool
isUpdateValid ruleMap update = fst $ checkValid (head nums) (True, tail nums)
  where
    nums = parseUpdate update
    checkValid curr (valid, remaining)
      | not valid = (False, [])
      | null remaining = (True, [])
      | otherwise =
          let nextValRule = Map.findWithDefault [] curr ruleMap
              nextNum = head remaining
           in if nextNum `elem` nextValRule
                then checkValid nextNum (True, tail remaining)
                else (False, [])

fixUpdateOrder :: Map Int [Int] -> String -> [Int]
fixUpdateOrder ruleMap update = topoSort nums graph
  where
    nums = parseUpdate update
    graph =
      Map.filterWithKey (\k _ -> k `elem` nums) $
        Map.map (filter (`elem` nums)) ruleMap

    topoSort :: [Int] -> Map Int [Int] -> [Int]
    topoSort nodes graph = fst $ foldl visit ([], Set.empty) nodes
      where
        visit (sorted, visited) node
          | node `Set.member` visited = (sorted, visited)
          | otherwise =
              let (newSorted, newVisited) =
                    foldl
                      visit
                      (sorted, Set.insert node visited)
                      (Map.findWithDefault [] node graph)
               in (node : newSorted, newVisited)

getMiddleElement :: [a] -> a
getMiddleElement update = update !! (length update `div` 2)

testPath :: String
testPath = "day5/test.txt"

finalPath :: String
finalPath = "day5/input.txt"

partOne :: IO ()
partOne = do
  let solution input =
        let (rules, updates) = parseRulesAndUpdate input
            ruleMap = buildRuleMap rules
            validUpdates = filter (isUpdateValid ruleMap) updates
         in sum . map (getMiddleElement . parseUpdate) $ validUpdates

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution input =
        let (rules, updates) = parseRulesAndUpdate input
            ruleMap = buildRuleMap rules
            invalidUpdates = filter (isUpdateValid ruleMap) updates
         in sum . map (getMiddleElement . fixUpdateOrder ruleMap) $ invalidUpdates

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
