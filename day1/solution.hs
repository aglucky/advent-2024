{-
Advent of Code 2024 Day 1
Adam Gluck
-}

import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List (sort)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
    handle <- openFile filePath ReadMode
    hGetContents handle

splitStringAt :: Char -> String -> [String]
splitStringAt delimiter input = case dropWhile (== delimiter) input of
    "" -> []
    remaining -> match : splitStringAt delimiter restOfString
        where (match, restOfString) = break (== delimiter) remaining

parseLists :: [String] -> ([Int], [Int])
parseLists lines = do
    let pairs = map (map read . splitStringAt ' ') lines
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

getCountsOfNum :: Int -> [Int] -> Int
getCountsOfNum _ [] = 0
getCountsOfNum toMatch (head:rest) = 
    if toMatch == head 
        then 1 + getCountsOfNum toMatch rest 
        else getCountsOfNum toMatch rest

getSimilarityScore :: ([Int], [Int]) -> Int
getSimilarityScore (leftList, rightList) = 
    sum $ map (\num -> num * getCountsOfNum num rightList) leftList

partOne :: IO ()
partOne = do
    testInput <- readInputFile "day1/test.txt"
    putStrLn $ "First Part Test Solution: " ++ show (getDistanceSum $ parseLists $ splitStringAt '\n' $ testInput)
    finalInput <- readInputFile "day1/input.txt"
    putStrLn $ "First Part Final Solution: " ++ show (getDistanceSum $ parseLists $ splitStringAt '\n' $ finalInput)

partTwo :: IO ()
partTwo = do
    testInput <- readInputFile "day1/test.txt"
    putStrLn $ "Second Part Test Solution: " ++ show (getSimilarityScore $ parseLists $ splitStringAt '\n' $ testInput)
    finalInput <- readInputFile "day1/input.txt"
    putStrLn $ "Second Part Final Solution: " ++ show (getSimilarityScore $ parseLists $ splitStringAt '\n' $ finalInput)

main :: IO ()
main = do
    partOne
    putStrLn "---"
    partTwo
