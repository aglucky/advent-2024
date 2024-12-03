{-
Advent of Code 2024 Day 3
Adam Gluck
-}

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Regex.Posix (getAllTextMatches, (=~))
import Data.List (isPrefixOf)

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

parseResults :: String -> [Token]
parseResults str =
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

partOne :: IO ()
partOne = do
  let solution = findMulSum . parseMulPairs

  testInput <- readInputFile "day3/test.txt"
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile "day3/input.txt"
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution = sumWithoutInvalid . parseResults

  testInput <- readInputFile "day3/test.txt"
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile "day3/input.txt"
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
