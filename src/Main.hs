module Main where

import Control.Concurrent (threadDelay)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, fromList, mapWithIndex)
import System.IO (IOMode(..), getLine, hGetContents, openFile, hFlush, stdout, hClose)

type Board = Seq Row

type Row = Seq Cell

data Cell
  = Alive
  | Dead

type Position = (Int, Int)

-- IO
main :: IO ()
main = do
  putStr "Enter path of file with pattern: "
  hFlush stdout
  filePath <- getLine
  file <- openFile filePath ReadMode
  content <- hGetContents file
  let board = parseBoard content
  printWorldLoop board 1
  hClose file

printWorldLoop :: Board -> Int -> IO ()
printWorldLoop board generation = do
  moveToTop
  clearScreen
  putStrLn $ "Generation: " ++ show generation ++ "\n"
  let nextBoard = nextWorld board
  putStrLn . showBoard $ nextBoard
  threadDelay 1000000 -- wait 1 second
  printWorldLoop nextBoard (generation + 1)

moveToTop :: IO ()
moveToTop = putStr "\ESC[H"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Parsing
parseBoard :: String -> Board
parseBoard s =
  if length nums < 2
    then Seq.empty
    else board
  where
    nums = fmap read (words s) :: [Int]
    width:height:rawBoard = nums
    rows = splitEvery width rawBoard
    board = numsToBoard rows

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = splitAt n list

showBoard :: Board -> String
showBoard = intercalate "\n" . toList . fmap showRow
  where
    showRow :: Row -> String
    showRow = unwords . toList . fmap showCell

showCell :: Cell -> String
showCell Alive = "O"
showCell Dead = " "

numsToBoard :: [[Int]] -> Board
numsToBoard = fromList . fmap (fromList . fmap numToCell)

numToCell :: Int -> Cell
numToCell 1 = Alive
numToCell _ = Dead

-- Next world logic
nextWorld :: Board -> Board
nextWorld board
  | height == 0 || width == 0 = board
  | otherwise = mapWithIndex mapRow board
  where
    height = Seq.length board
    width = Seq.length firstRow
    firstRow = Seq.index board 0
    mapRow x = mapWithIndex (\y cell -> mapCell (x, y) cell)
    mapCell (x, y) cell = nextCell cell (x, y) board

nextCell :: Cell -> Position -> Board -> Cell
nextCell cell position board = nextCell' cell (neighboursAlive position board)
  where
    nextCell' :: Cell -> Int -> Cell
    -- get next cell by providing a cell and count of neighbours alive
    nextCell' Dead 3 = Alive
    nextCell' Alive 2 = Alive
    nextCell' Alive 3 = Alive
    nextCell' _ _ = Dead

neighboursAlive :: Position -> Board -> Int
neighboursAlive (x, y) board =
  length . filter id . fmap neighbourAlive $ relativeNeighbourPositions
  where
    neighbourAlive :: Position -> Bool
    neighbourAlive (nx, ny) = cellAlive (x + nx, y + ny) board

cellAlive :: Position -> Board -> Bool
cellAlive (x, y) board =
  case Seq.lookup x board of
    Just row ->
      case Seq.lookup y row of
        Just Alive -> True
        _ -> False
    _ -> False

relativeNeighbourPositions =
  [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]
