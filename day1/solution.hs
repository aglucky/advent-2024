{-
Advent of Code 2024 Day 1
Adam Gluck
-}

import Data.Char (toLower)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

parseLists :: [String] -> ([Int], [Int])
parseLists lines = do
  let pairs = map (map read . words) lines
  let filteredPairs = filter (\pair -> length pair == 2) pairs
  if length filteredPairs /= length pairs
    then error "All pairs must have length 2"
    else (map head filteredPairs, map last filteredPairs)

sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (xs, ys) = (sort xs, sort ys)

getDistanceSum :: ([Int], [Int]) -> Int
getDistanceSum (left, right) =
  if length left /= length right
    then error "Lists must have the same length"
    else
      let (sortedLeft, sortedRight) = sortLists (left, right)
       in sum $ zipWith (\x y -> abs (x - y)) sortedLeft sortedRight

makeNumCountMap :: [Int] -> Map.Map Int Int
makeNumCountMap = foldr (\num acc -> Map.insertWith (+) num 1 acc) Map.empty

getSimilarityScore :: ([Int], [Int]) -> Int
getSimilarityScore (leftList, rightList) =
  let rightCounts = makeNumCountMap rightList
   in sum . map (\num -> num * Map.findWithDefault 0 num rightCounts) $ leftList

testPath :: String
testPath = "day1/test.txt"

finalPath :: String
finalPath = "day1/input.txt"

partOne :: IO ()
partOne = do
  let solution = getDistanceSum . parseLists . lines

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution = getSimilarityScore . parseLists . lines

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
