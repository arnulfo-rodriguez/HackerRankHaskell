{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

{-|
Module      : Bomberman
Description : Solution for the Bomberman Game simulation problem from HackerRank

== Problem Statement

Bomberman lives in a rectangular grid. Each cell is either empty ('.') or contains 
a bomb ('O'). The game follows these rules:

1. Initially, Bomberman arbitrarily plants bombs in some cells (time = 0)
2. After 1 second, Bomberman does nothing
3. After 2 seconds, Bomberman plants bombs in ALL empty cells (they start with timer = 0)
4. After 3 seconds, bombs planted at t=0 explode, clearing themselves and adjacent cells
5. This pattern continues: plant on even seconds, explode bombs that are 3 seconds old

When a bomb explodes:
- The cell containing the bomb becomes empty
- All 4 adjacent cells (up, down, left, right) become empty
- Bombs in adjacent cells are destroyed without exploding

Given n seconds and initial grid, determine the grid state after n seconds.

== Algorithm Overview

The naive approach would simulate each second, but for large n (up to 10^9), 
this is impractical. The key insight is that the grid states form a cycle:

1. The grid alternates between specific patterns
2. After a few iterations, the state repeats
3. We can detect this cycle using memoization and skip ahead

The solution:
- Simulate each tick while storing visited states in a Map
- When we encounter a previously seen state, we've found a cycle
- Use modular arithmetic to determine the final state without full simulation

== Data Types

- Cell: Represents a grid cell
  - EmptyCell row col: An empty position
  - Planted age row col: A bomb planted 'age' seconds ago at (row, col)

- Matrix: Represents the grid state
  - Contains dimensions (rows, cols) and an Array of Cells
  - Two Matrix values are equal if they have the same cell configuration

- MatricesState: State for memoization
  - Tracks remaining iterations and a Map of (current_state -> next_state)

== Time Complexity

O(R * C * K) where K is the cycle length (typically very small, < 10)
Space: O(R * C * K) to store the cycle states

-}
module Bomberman(bombermanMain) where
import Control.Monad
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
import Data.Map as Map
import Control.Monad.State (State, runState, evalState, modify, get, lift)

--
-- Complete the 'bomberMan' function below.
--
-- The function is expected to return a STRING_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. STRING_ARRAY grid
--

-- | Represents a single cell in the grid
-- EmptyCell: An empty position at (row, col)
-- Planted: A bomb at (row, col) that has been planted for 'age' seconds
data Cell = Planted Int Int Int | EmptyCell Int Int deriving (Eq,Show,Ord)

-- | Represents the entire grid state
-- Matrix rows cols cellArray
data Matrix = Matrix Int Int (Array Int Cell) deriving (Eq, Show,Ord)

-- | State for tracking simulation progress and detecting cycles
-- MatricesState remainingIterations stateMap
-- where stateMap stores (current_state -> next_state) transitions
data MatricesState = MatricesState Int (Map Matrix Matrix) deriving Show

-- | Converts a flat array index to 2D coordinates (row, col)
-- and applies a constructor function with those coordinates
indexToPosition :: Int -> Int -> (Int -> Int -> a) -> a
indexToPosition columns index  constructor =
    let row = index `div` columns
        col = index `mod` columns
    in constructor row col

-- | Converts a character from the input string to a Cell
-- '.' becomes EmptyCell, 'O' becomes Planted with age 0
fromString '.' columns index = indexToPosition columns index EmptyCell
fromString 'O' columns index  = indexToPosition columns index (Planted 0)

-- | Converts a Cell to its character representation
-- EmptyCell -> '.', Planted -> 'O'
toString (EmptyCell _ _) = '.'
toString (Planted {}) = 'O'

-- | Parses a string grid into a Matrix data structure
-- Takes rows, columns, and list of strings representing the grid
stringMatrixToGrid :: Int -> Int -> [String] -> Matrix
stringMatrixToGrid rows cols matrix =
  let
    flattenedMatrix = Data.List.concat matrix
    theArray = array (0 , rows * cols - 1) [(i, fromString x cols i) | (i,x) <- Data.List.zip [0..] flattenedMatrix]
  in Matrix rows cols theArray

-- | Helper to convert a flat array back to 2D rows
toRows cols rows arr = [[arr Data.Array.! (row * cols + col) | col <- [0..cols-1]] | row <- [0..rows-1]]

-- | Converts a Matrix back to a list of strings for output
gridToStringMatrix:: Matrix -> [String]
gridToStringMatrix (Matrix rows cols theData) =  toRows cols rows $ fmap toString theData

-- | Plants a bomb in a cell if it's empty; leaves planted bombs unchanged
plantCell :: Cell -> Cell
plantCell (EmptyCell i j) = Planted 0 i j
plantCell p@(Planted {}) = p

-- | Plants bombs in all empty cells of the grid
-- This simulates Bomberman planting bombs on even seconds
plantGrid :: Matrix -> Matrix
plantGrid (Matrix x y theData) = Matrix x y (fmap plantCell theData)

-- | Explodes all bombs that have been planted for 3 seconds
-- Each explosion clears the bomb's cell and all 4 adjacent cells
explodeGrid :: Matrix -> Matrix
explodeGrid matrix@(Matrix x y theData) =
    let
        willExplode (Planted t _ _) = t == 3
        willExplode _  = False
        toExplode = Data.List.filter willExplode (Data.Array.elems theData)
    in
        Data.Foldable.foldl  (flip explodeCell) matrix toExplode

-- | Replaces a cell at position (x, y) with a new cell
-- Returns the matrix unchanged if coordinates are out of bounds
replaceCell x y newCell m@(Matrix rows cols theData)
 | x < 0 || x >= rows || y < 0 || y >= cols = m
 | otherwise =
    let
        thePosition = x * cols + y
        newTheData =  theData // [(thePosition, newCell)]
    in  Matrix rows cols newTheData

-- | Returns a list of (row, col) coordinates for a cell and its 4 adjacent neighbors
-- Filters out coordinates that would be outside the grid bounds
cellAndNeighbors x y (Matrix rows cols _) =  (x,y):Data.List.filter (\ (i,j) -> i >= 0 && i < rows && j >= 0  && j < cols)  [(x - 1, y), (x + 1, y), (x , y - 1) , (x, y + 1)]

-- | Explodes a single bomb, clearing it and all adjacent cells
-- Does nothing if the cell is empty
explodeCell :: Cell -> Matrix -> Matrix
explodeCell (EmptyCell _ _) grid = grid
explodeCell p@(Planted plantedTime x y) matrix@(Matrix rows cols _)  = Data.List.foldl (\ accMatrix (i,j) -> replaceCell i j (EmptyCell i j) accMatrix) matrix (cellAndNeighbors x y matrix)

-- | Advances time by 1 second for all planted bombs
-- Empty cells remain unchanged
advanceTime (Matrix rows cols theData) =
    let
      advanceCellTime c@(EmptyCell _ _ ) = c
      advanceCellTime (Planted delta x y) = Planted (delta + 1) x y
      advanceMatrixTime = fmap advanceCellTime theData
    in Matrix rows cols advanceMatrixTime

-- | Applies game rules for a given tick:
-- Even ticks: Plant bombs in all empty cells
-- Odd ticks: Explode all bombs that are 3 seconds old
iterateBomber ticks g
  | even ticks = plantGrid g
  | otherwise = explodeGrid g

-- | Detects a cycle in the state sequence
-- Starting from a seed matrix, follows the state transitions until
-- we return to the seed, building a list of all states in the cycle
-- Returns (cycle_states, cycle_length)
matrixMapToList :: Matrix -> Map Matrix Matrix -> ([Matrix],Int)
matrixMapToList seed matrices =
  matrixMapToSequenceRec seed 0 []
  where
    matrixMapToSequenceRec current count result
      | (Data.Foldable.null result || current /= seed) && Map.member current matrices = matrixMapToSequenceRec (matrices Map.! current) (count + 1) (result ++ [current])
      | otherwise = (result,count)

-- | Main simulation function using State monad
-- Simulates the game tick by tick, memoizing states
-- When a cycle is detected, uses modular arithmetic to skip to the final state
-- This allows handling very large n values efficiently
handleTick :: Matrix -> Int -> State MatricesState Matrix
handleTick matrix ticks =
    let
        tickHandler m = iterateBomber ticks (advanceTime m)
    in
        do
          memo <- get
          case memo of
             (MatricesState iterations seq) ->
                 case Map.lookup matrix seq of
                 Nothing -> do
                                let nextMatrix = tickHandler matrix
                                modify $ \_ -> MatricesState (iterations - 1) (Map.insert matrix nextMatrix seq)
                                (handleTick nextMatrix (ticks + 1))
                 Just v -> do
                                let (stateAsList,len) = matrixMapToList matrix seq
                                let modularResult = iterations `mod` len
                                let result = stateAsList Data.List.!! modularResult
                                return result

-- | Main entry point for the Bomberman simulation
-- Takes: n (seconds), rows, cols, initial grid as strings
-- Returns: (final grid as strings, memoization state)
-- The memoization state is returned for debugging/analysis
bomberMan :: Int -> Int -> Int -> [String] -> ([String],MatricesState)
bomberMan n rows cols strGrid =
  let
    grid = stringMatrixToGrid rows cols strGrid
    initialState = MatricesState n Map.empty
    (lastGrid,s) = runState (handleTick grid 1) initialState
    result = gridToStringMatrix lastGrid
  in (result,s)

-- | String utility: strip leading whitespace
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
-- | String utility: strip trailing whitespace
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

-- | Read n lines from stdin and return as a list of strings
readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray (n - 1)
    return (line : rest)

-- | Main IO function for HackerRank submission
-- Reads input, runs simulation, and writes output to specified file
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

    let (result,(MatricesState _ s)) = bomberMan n r c grid

    hPutStrLn fptr $ Data.List.intercalate "\n" result
    hPrint fptr (show (Map.size s))

    hFlush fptr
    hClose fptr