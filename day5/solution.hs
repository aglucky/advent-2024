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
buildRuleMap = foldl (\acc (before, after) -> Map.insertWith (++) before [after] acc) Map.empty . map parseRule

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
          let nextVals = Map.findWithDefault [] curr ruleMap
              nextNum = head remaining
           in if nextNum `elem` nextVals
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
    topoSort vertices graph = fst $ foldl visit ([], Set.empty) vertices
      where
        visit (sorted, visited) v
          | v `Set.member` visited = (sorted, visited)
          | otherwise =
              let (newSorted, newVisited) =
                    foldl
                      visit
                      (sorted, Set.insert v visited)
                      (Map.findWithDefault [] v graph)
               in (v : newSorted, newVisited)

testPath :: String
testPath = "day5/test.txt"

finalPath :: String
finalPath = "day5/input.txt"

partOne :: IO ()
partOne = do
  let solution input =
        let (rules, updates) = parseRulesAndUpdate input
            ruleMap = buildRuleMap rules
            getMiddleElement update = update !! (length update `div` 2)
         in sum . map (getMiddleElement . parseUpdate) . filter (isUpdateValid ruleMap) $ updates

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution input =
        let (rules, updates) = parseRulesAndUpdate input
            ruleMap = buildRuleMap rules
            getMiddleElement update = update !! (length update `div` 2)
         in sum . map (getMiddleElement . fixUpdateOrder ruleMap) . filter (not . isUpdateValid ruleMap) $ updates

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
