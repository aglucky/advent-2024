{-
Advent of Code 2024 Day 4
Adam Gluck
-}

import Data.List (isPrefixOf, transpose)
import Data.Maybe (fromJust, isJust)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

getDiagonalLines :: [String] -> [String]
getDiagonalLines grid =
  [getDiagonalLine i 0 grid | i <- [0 .. rowLen - 1]]
    ++ [getDiagonalLine 0 j grid | j <- [1 .. colLen - 1]]
    ++ [getDiagonalLine i 0 flippedGrid | i <- [0 .. rowLen - 1]]
    ++ [getDiagonalLine 0 j flippedGrid | j <- [1 .. colLen - 1]]
  where
    flippedGrid = map reverse grid
    rowLen = length grid
    colLen = length $ head grid

    getDiagonalLine i j grid =
      [(grid !! (i + k)) !! (j + k) | k <- [0 .. min (rowLen - 1 - i) (colLen - 1 - j)]]

numWordsInLine :: String -> String -> Int
numWordsInLine word line =
  if null line
    then 0
    else
      (if word `isPrefixOf` line then 1 else 0)
        + numWordsInLine word (tail line)

numWordsInGrid :: [String] -> Int
numWordsInGrid grid =
  sum [sum (map xmasCount grid) + sum (map (xmasCount . reverse) grid) | grid <- grids]
  where
    xmasCount = numWordsInLine "XMAS"
    grids = [grid, transpose grid, getDiagonalLines grid]

getAllNeighbors :: [String] -> [(Int, Int)] -> [[Maybe Char]]
getAllNeighbors grid = map (getNeighbors grid)
  where
    rowLen = length grid
    colLen = length . (!!) grid
    getNeighbors grid (i, j) =
      [ safeGet (i - 1) (j - 1),
        safeGet (i - 1) (j + 1),
        safeGet (i + 1) (j - 1),
        safeGet (i + 1) (j + 1)
      ]
      where
        safeGet row col
          | row >= 0 && row < rowLen && col >= 0 && col < colLen row =
              Just ((grid !! row) !! col)
          | otherwise = Nothing

findAPositions :: [String] -> [(Int, Int)]
findAPositions grid =
  [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], (grid !! i) !! j == 'A']
  where
    n = length grid

getNumValidSquares :: [[Maybe Char]] -> Int
getNumValidSquares candidates =
  let isNotNull = all isJust
      toChars = map fromJust
      isValid candidate = candidate `elem` ["MSMS", "MMSS", "SSMM", "SMSM"]
   in length $ filter (\x -> isNotNull x && isValid (toChars x)) candidates

testPath :: String
testPath = "day4/test.txt"

finalPath :: String
finalPath = "day4/input.txt"

partOne :: IO ()
partOne = do
  let solution = numWordsInGrid . lines

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution input =
        let grid = lines input
         in getNumValidSquares . getAllNeighbors grid $ findAPositions grid

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
