{-
Advent of Code 2024 Day 3
Adam Gluck
-}

import Data.List (isPrefixOf)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Regex.Posix (getAllTextMatches, (=~))

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

parseMulPairs :: String -> [(Int, Int)]
parseMulPairs str =
  let mulPattern = "mul\\(([1-9][0-9]*),([1-9][0-9]*)\\)"
      matches = str =~ mulPattern :: [[String]]
      numberPairs = [(read (x !! 1), read (x !! 2)) | x <- matches]
   in numberPairs

findMulSum :: [(Int, Int)] -> Int
findMulSum = sum . map (uncurry (*))

data Token = MulPair Int Int | Do | Dont
  deriving (Show)

parseTokens :: String -> [Token]
parseTokens str =
  let mulPattern = "mul\\(([1-9][0-9]*),([1-9][0-9]*)\\)|do\\(\\)|don't\\(\\)"
      matches = getAllTextMatches (str =~ mulPattern) :: [String]

      parseMatch match
        | "mul(" `isPrefixOf` match =
            let nums = match =~ "([1-9][0-9]*),([1-9][0-9]*)" :: [[String]]
                [_, n1, n2] = head nums
             in MulPair (read n1) (read n2)
        | "do()" == match = Do
        | "don't()" == match = Dont
        | otherwise = error "Invalid pattern match"
   in map parseMatch matches

sumWithoutInvalid :: [Token] -> Int
sumWithoutInvalid results =
  let (sum, _) = foldl processResult (0, True) results
   in sum
  where
    processResult (curSum, _) Do = (curSum, True)
    processResult (curSum, _) Dont = (curSum, False)
    processResult (curSum, True) (MulPair x y) = (curSum + (x * y), True)
    processResult (curSum, False) _ = (curSum, False)

testPath :: String
testPath = "day3/test.txt"

finalPath :: String
finalPath = "day3/input.txt"

partOne :: IO ()
partOne = do
  let solution = findMulSum . parseMulPairs

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution = sumWithoutInvalid . parseTokens

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
