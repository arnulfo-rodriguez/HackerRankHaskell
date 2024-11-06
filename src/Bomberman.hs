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
import Data.Sequence as Seq
import Data.Foldable

--
-- Complete the 'bomberMan' function below.
--
-- The function is expected to return a STRING_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. STRING_ARRAY grid
--
data Cell = Planted Int | EmptyCell
data Matrix = Matrix Int Int (Seq (Seq Cell))
data BombermanStates = DoNothing | Plant


fromString '.' = EmptyCell
fromString 'O' = Planted 0

toString EmptyCell = '.'
toString (Planted _) = 'O'

nextState DoNothing = Plant
nextState Plant = DoNothing



mapNested f = fmap (fmap f)

stringMatrixToGrid :: Int -> Int -> [String] -> Matrix
stringMatrixToGrid rows cols matrix = Matrix rows cols $  Seq.fromList $  Data.List.map Seq.fromList $ mapNested fromString matrix


gridToStringMatrix:: Matrix -> [String]
gridToStringMatrix (Matrix _ _ theData) =   Data.Foldable.toList $  (Data.Foldable.toList <$> mapNested toString theData)

plantCell :: Int -> Cell -> Cell
plantCell ticks EmptyCell = Planted ticks
plantCell _ p@(Planted _) = p

plantGrid :: Int -> Matrix -> Matrix
plantGrid ticks (Matrix x y theData) = Matrix x y (mapNested (plantCell ticks) theData)

explodeGrid :: Int -> Matrix -> Matrix
explodeGrid ticks matrix@(Matrix x y theData) =
    let
        mapRow :: Int -> Seq Cell -> Seq (Int, Int, Cell)
        mapRow i = Seq.mapWithIndex (\ j cell -> (i, j, cell))
        mappedMatrix =  Data.Foldable.foldl (\ l row ->  Data.Foldable.toList row ++ l) [] $ Seq.mapWithIndex mapRow theData
        willExplode (Planted t) = (ticks - t) == 3
        willExplode _  = False
        toExplode = Data.List.filter (\ (_,_,c) -> willExplode c) mappedMatrix
    in
        Data.Foldable.foldl  (\ m (i, j, cell) -> explodeCell i j cell ticks m)  matrix toExplode


replaceCell x y newCell m@(Matrix rows cols theData)
 | x < 0 || x >= rows || y < 0 || y >= cols = m
 | otherwise =
    let
        theRow = Seq.index theData x
        newRow = Seq.update y newCell theRow
        newTheData = Seq.update x newRow theData
    in  Matrix rows cols newTheData


cellAndNeighbors x y (Matrix rows cols _) =  (x,y):Data.List.filter (\ (i,j) -> i >= 0 && i < rows && j >= 0  && j < cols)  [(x - 1, y), (x + 1, y), (x , y - 1) , (x, y + 1)]

explodeCell :: Int -> Int -> Cell -> Int -> Matrix -> Matrix
explodeCell _ _ EmptyCell _ grid = grid
explodeCell x y p@(Planted plantedTime) ticks matrix@(Matrix rows cols _)  = Data.List.foldl (\ accMatrix (i,j) -> replaceCell i j EmptyCell accMatrix) matrix (cellAndNeighbors x y matrix)

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
