{-
Advent of Code 2024 Day 2
Adam Gluck
-}

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

maxDistance :: Int
maxDistance = 3

isLevelValid :: [Int] -> Bool
isLevelValid (first : second : rest) =
  if first < second
    then isLevelValidInner (first : second : rest) (<)
    else isLevelValidInner (first : second : rest) (>)
isLevelValid _ = True

isLevelValidInner :: [Int] -> (Int -> Int -> Bool) -> Bool
isLevelValidInner (first : second : rest) sameDir =
  sameDir first second
    && abs (first - second) < 4
    && first /= second
    && isLevelValidInner (second : rest) sameDir
isLevelValidInner _ _ = True

isLevelValidWithReplaceInner :: [Int] -> (Int -> Int -> Bool) -> Bool
isLevelValidWithReplaceInner (first : second : third : rest) sameDir =
  if sameDir first second
    && sameDir second third
    && abs (first - second) <= maxDistance
    && abs (second - third) <= maxDistance
    && first /= second
    && second /= third
    then isLevelValidWithReplaceInner (second : third : rest) sameDir
    else
      isLevelValidInner (first : third : rest) sameDir
        || isLevelValidInner (second : third : rest) sameDir
        || isLevelValidInner (first : second : rest) sameDir
isLevelValidWithReplaceInner _ _ = True

isLevelValidWithReplace :: [Int] -> Bool
isLevelValidWithReplace (first : second : third : rest) =
  if abs (first - second) <= maxDistance
    && abs (second - third) <= maxDistance
    && first /= second
    && second /= third
    then
      case (compare first second, compare second third) of
        (LT, LT) -> isLevelValidWithReplaceInner (first : second : third : rest) (<)
        (GT, GT) -> isLevelValidWithReplaceInner (first : second : third : rest) (>)
        _ -> tryWithReplacement [first, second, third] rest
    else tryWithReplacement [first, second, third] rest
  where
    tryWithReplacement [a, b, c] rest =
      isLevelValid (a : c : rest)
        || isLevelValid (b : c : rest)
        || isLevelValid (a : b : rest)
isLevelValidWithReplace (first : second : rest) = abs (first - second) <= maxDistance
isLevelValidWithReplace _ = True

numValidLevels :: ([Int] -> Bool) -> [[Int]] -> Int
numValidLevels isValid levels = length $ filter isValid levels

testPath :: String
testPath = "day2/test.txt"

finalPath :: String
finalPath = "day2/input.txt"

partOne :: IO ()
partOne = do
  let solution = numValidLevels isLevelValid . parseLevels

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution = numValidLevels isLevelValidWithReplace . parseLevels

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
