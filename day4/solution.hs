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

parseWordSearch :: String -> [String]
parseWordSearch = lines

getDiagonalLines :: [String] -> [String]
getDiagonalLines grid =
  [getDiagonalLine i 0 grid | i <- [0 .. n - 1]]
    ++ [getDiagonalLine 0 j grid | j <- [1 .. n - 1]]
    ++ [getReverseDiagonalLine i (n - 1) grid | i <- [0 .. n - 1]]
    ++ [getReverseDiagonalLine 0 j grid | j <- [0 .. n - 2]]
  where
    n = length grid
    getDiagonalLine i j grid =
      [(grid !! (i + k)) !! (j + k) | k <- [0 .. min (n - 1 - i) (n - 1 - j)]]
    getReverseDiagonalLine i j grid =
      [(grid !! (i + k)) !! (j - k) | k <- [0 .. min (n - 1 - i) j]]

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

getNeighbors :: [String] -> [(Int, Int)] -> [(Maybe Char, Maybe Char, Maybe Char, Maybe Char)]
getNeighbors grid = map (getAdjacentChars grid)
  where
    n = length grid
    getAdjacentChars grid (i, j) =
      ( safeGet (i - 1) (j - 1),
        safeGet (i - 1) (j + 1),
        safeGet (i + 1) (j - 1),
        safeGet (i + 1) (j + 1)
      )
      where
        safeGet row col
          | row >= 0 && row < n && col >= 0 && col < n = Just ((grid !! row) !! col)
          | otherwise = Nothing

findAPositions :: [String] -> [(Int, Int)]
findAPositions grid =
  [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1], (grid !! i) !! j == 'A']
  where
    n = length grid

getNumValidSquares :: [(Maybe Char, Maybe Char, Maybe Char, Maybe Char)] -> Int
getNumValidSquares candidates =
  let isNotNull (a, b, c, d) = all isJust [a, b, c, d]
      toChars (a, b, c, d) = [fromJust a, fromJust b, fromJust c, fromJust d]
      isValid candidate = candidate `elem` ["MSMS", "MMSS", "SSMM", "SMSM"]
   in length $ filter (\x -> isNotNull x && isValid (toChars x)) candidates

testPath :: String
testPath = "day4/test.txt"

finalPath :: String
finalPath = "day4/input.txt"

partOne :: IO ()
partOne = do
  let solution = numWordsInGrid . parseWordSearch

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution input =
        let grid = parseWordSearch input
         in getNumValidSquares . getNeighbors grid $ findAPositions grid

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
