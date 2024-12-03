{-
Advent of Code 2024 Day 4
Adam Gluck
-}

import Data.List (isPrefixOf)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Regex.Posix (getAllTextMatches, (=~))

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle


testPath :: String
testPath = "day4/test.txt"

finalPath :: String
finalPath = "day4/input.txt"

partOne :: IO ()
partOne = do
  let solution = id

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution = id

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
