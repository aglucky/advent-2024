{-
Advent of Code 2024 Day 2
Adam Gluck
-}

import Control.Arrow (Arrow (second))
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

splitStringAt :: Char -> String -> [String]
splitStringAt delimiter input = case dropWhile (== delimiter) input of
  "" -> []
  remaining -> match : splitStringAt delimiter restOfString
    where
      (match, restOfString) = break (== delimiter) remaining

parseLevels :: String -> [[Int]]
parseLevels input = map (map read . splitStringAt ' ') (splitStringAt '\n' input)

isLevelValid :: [Int] -> Bool
isLevelValid (first : second : rest) =
  if first < second
    then isLevelValidInner (first : second : rest) (<)
    else isLevelValidInner (first : second : rest) (>)
isLevelValid _ = True

isLevelValidInner :: [Int] -> (Int -> Int -> Bool) -> Bool
isLevelValidInner (first : second : rest) comp =
  comp first second
    && abs (first - second) < 4
    && first /= second
    && isLevelValidInner (second : rest) comp
isLevelValidInner _ _ = True

isLevelValidWithReplaceInner :: [Int] -> (Int -> Int -> Bool) -> Bool
isLevelValidWithReplaceInner (first : second : third : rest) comp =
  if comp first second
    && comp second third
    && abs (first - second) < 4
    && abs (second - third) < 4
    && first /= second
    && second /= third
    then isLevelValidWithReplaceInner (second : third : rest) comp
    else 
      isLevelValidInner (first : third : rest) comp
      || isLevelValidInner (second : third : rest) comp
      || isLevelValidInner (first : second : rest) comp
isLevelValidWithReplaceInner _ _ = True

isLevelValidWithReplace :: [Int] -> Bool
isLevelValidWithReplace (first : second : third : rest) =
  if abs (first - second) < 4 && abs (second - third) < 4 && first /= second && second /= third
    then
      if first < second && second < third
        then isLevelValidWithReplaceInner (first : second : third : rest) (<)
        else
          if first > second && second > third
            then isLevelValidWithReplaceInner (first : second : third : rest) (>)
            else tryWithoutOne [first, second, third] rest
    else tryWithoutOne [first, second, third] rest
  where
    tryWithoutOne [a, b, c] rest = isLevelValid (a : c : rest) || isLevelValid (b : c : rest) || isLevelValid (a : b : rest)
isLevelValidWithReplace (first : second: rest) = abs (first - second) < 4
isLevelValidWithReplace _ = True

numValidLevels :: ([Int] -> Bool) -> [[Int]] -> Int
numValidLevels isValid levels = length $ filter isValid levels

partOne :: IO ()
partOne = do
  testInput <- readInputFile "day2/test.txt"
  putStrLn $ "First Part Test Solution: " ++ show (numValidLevels isLevelValid . parseLevels $ testInput)

  finalInput <- readInputFile "day2/input.txt"
  putStrLn $ "First Part Final Solution: " ++ show (numValidLevels isLevelValid . parseLevels $ finalInput)

partTwo :: IO ()
partTwo = do
  testInput <- readInputFile "day2/test.txt"
  putStrLn $ "Second Part Test Solution: " ++ show (numValidLevels isLevelValidWithReplace . parseLevels $ testInput)

  finalInput <- readInputFile "day2/input.txt"
  putStrLn $ "Second Part Final Solution: " ++ show (numValidLevels isLevelValidWithReplace . parseLevels $ finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
