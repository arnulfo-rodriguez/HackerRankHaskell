{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Bomberman(bombermanMain) where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Text.Printf (printf)
import Data.Array as Array
import Data.Array (array, assocs, bounds, indices, elems, (!))
import Data.Foldable

--
-- Complete the 'bomberMan' function below.
--
-- The function is expected to return a STRING_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. STRING_ARRAY grid
--
data Cell = Planted Int Int Int | EmptyCell Int Int
data Matrix = Matrix Int Int (Array Int Cell)

indexToPosition :: Int -> Int -> (Int -> Int -> a) -> a
indexToPosition columns index  constructor =
    let row = index `div` columns
        col = index `mod` columns
    in (constructor row col)
    
fromString '.' columns index = indexToPosition columns index EmptyCell
fromString 'O' columns index  = indexToPosition columns index (Planted 0)

toString (EmptyCell _ _) = '.'
toString (Planted _ _ _) = 'O'

stringMatrixToGrid :: Int -> Int -> [String] -> Matrix
stringMatrixToGrid rows cols matrix = 
  let
    flattenedMatrix = Data.List.concat matrix
    theArray = array (0 , (rows * cols) - 1) [(i, fromString x cols i) | (i,x) <- Data.List.zip [0..] flattenedMatrix]
  in Matrix rows cols theArray

toRows cols rows arr = [[arr ! (row * cols + col) | col <- [0..cols-1]] | row <- [0..rows-1]]
                   
gridToStringMatrix:: Matrix -> [String]
gridToStringMatrix (Matrix rows cols theData) =  toRows cols rows $ fmap toString theData

plantCell :: Int -> Cell -> Cell
plantCell ticks (EmptyCell i j) = Planted ticks i j
plantCell _ p@(Planted _ _ _) = p

plantGrid :: Int -> Matrix -> Matrix
plantGrid ticks (Matrix x y theData) = Matrix x y (fmap (plantCell ticks) theData)

explodeGrid :: Int -> Matrix -> Matrix
explodeGrid ticks matrix@(Matrix x y theData) =
    let
        willExplode (Planted t _ _) = (ticks - t) == 3
        willExplode _  = False
        toExplode = Data.List.filter willExplode (Data.Array.elems theData)
    in
        Data.Foldable.foldl  (\ m cell -> explodeCell cell ticks m) matrix toExplode

replaceCell x y newCell m@(Matrix rows cols theData)
 | x < 0 || x >= rows || y < 0 || y >= cols = m
 | otherwise =
    let
        thePosition = (x * cols) + y
        newTheData =  theData // [(thePosition, newCell)]
    in  Matrix rows cols newTheData


cellAndNeighbors x y (Matrix rows cols _) =  (x,y):Data.List.filter (\ (i,j) -> i >= 0 && i < rows && j >= 0  && j < cols)  [(x - 1, y), (x + 1, y), (x , y - 1) , (x, y + 1)]

explodeCell :: Cell -> Int -> Matrix -> Matrix
explodeCell (EmptyCell _ _) _ grid = grid
explodeCell p@(Planted plantedTime x y) ticks matrix@(Matrix rows cols _)  = Data.List.foldl (\ accMatrix (i,j) -> replaceCell i j (EmptyCell i j) accMatrix) matrix (cellAndNeighbors x y matrix)

iterateBomber g ticks
  | even ticks = plantGrid ticks g
  | otherwise = explodeGrid ticks g

bomberMan :: Int -> Int -> Int -> [String] -> [String]
bomberMan n rows cols strGrid =
  let
    grid = stringMatrixToGrid rows cols strGrid
    lastGrid = Data.Foldable.foldl iterateBomber grid [2..n]
    result = gridToStringMatrix lastGrid
  in result

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

bombermanMain :: IO()
bombermanMain = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let r = read (Data.List.head firstMultipleInput) :: Int

    let c = read (firstMultipleInput !! 1) :: Int

    let n = read (firstMultipleInput !! 2) :: Int

    grid <- readMultipleLinesAsStringArray r

    let result = bomberMan n r c grid

    hPutStrLn fptr $ Data.List.intercalate "\n" result
  
    hFlush fptr
    hClose fptr