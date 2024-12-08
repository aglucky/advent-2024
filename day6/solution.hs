{-
Advent of Code 2024 Day 6
Adam Gluck
-}

import Data.Array.Unboxed (UArray, amap, bounds, inRange, listArray, (!), (//))
import Data.Maybe (isJust)
import Data.Set qualified as Set
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

type Coord = (Int, Int)
type Dir = (Int, Int)

north, south, east, west :: Dir
north = (-1, 0)
south = (1, 0)
east = (0, 1)
west = (0, -1)

turnRight :: Dir -> Dir
turnRight (-1, 0) = (0, 1)
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)

createGrid :: String -> UArray Coord Char
createGrid input =
  let grid = lines input
      rows = length grid
      cols = length (head grid)
      bounds = ((0, 0), (rows - 1, cols - 1))
  in listArray bounds $ concat grid

findStart :: UArray Coord Char -> Coord
findStart array = 
  let ((_, _), (rows, cols)) = bounds array
  in head [(i, j) | i <- [0 .. rows], j <- [0 .. cols], array ! (i, j) == '^']

getUniqueElems :: (Ord a) => [a] -> [a]
getUniqueElems = removeDuplicates Set.empty
  where
    removeDuplicates seenElems [] = []
    removeDuplicates seenElems (currentElement : remainingElems)
      | currentElement `Set.member` seenElems = removeDuplicates seenElems remainingElems
      | otherwise = currentElement : removeDuplicates (Set.insert currentElement seenElems) remainingElems

isLoop :: (Ord a) => [a] -> Bool
isLoop elements = checkForLoop elements elements
  where
    checkForLoop (firstElem : restElems) (_ : secondElem : remainingElems) =
      firstElem == secondElem || checkForLoop restElems remainingElems
    checkForLoop _ _ = False

(!?) :: UArray Coord Bool -> Coord -> Maybe Bool
arr !? pos@(y, x)
  | inRange (bounds arr) pos = Just (arr ! pos)
  | otherwise = Nothing

movePos :: Dir -> Coord -> Coord
movePos (dx, dy) (x, y) = (dx + x, dy + y)

walk :: UArray (Int, Int) Bool -> Dir -> Coord -> [(Dir, Coord)]
walk grid dir pos =
  (dir, pos)
    : case grid !? nextP of
      Nothing -> []
      Just True -> walk grid (turnRight dir) pos
      Just False -> walk grid dir (movePos dir pos)
  where
    nextP = movePos dir pos

testPath :: String
testPath = "day6/test.txt"

finalPath :: String
finalPath = "day6/input.txt"

partOne :: IO ()
partOne = do
  let solution input = do
        let grid = createGrid input
            start = findStart grid
            walls = amap (== '#') grid
            path = getUniqueElems (map snd (walk walls north start))
        length path

  testInput <- readInputFile testPath
  putStrLn $ "First Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "First Part Final Solution: " ++ show (solution finalInput)

partTwo :: IO ()
partTwo = do
  let solution input = do
        let grid = createGrid input
            start = findStart grid
            walls = amap (== '#') grid
            path = getUniqueElems (map snd (walk walls north start))
            check pos = isLoop (walk (walls // [(pos, True)]) north start)
        length . filter id . map check $ tail path

  testInput <- readInputFile testPath
  putStrLn $ "Second Part Test Solution: " ++ show (solution testInput)
  finalInput <- readInputFile finalPath
  putStrLn $ "Second Part Final Solution: " ++ show (solution finalInput)

main :: IO ()
main = do
  partOne
  putStrLn "---"
  partTwo
