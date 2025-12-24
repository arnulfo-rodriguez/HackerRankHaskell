{-|
Module      : SuperQueens
Description : Solver for the Super Queens problem
Copyright   : (c) 2025
License     : MIT

This module solves the Super Queens problem, a variant of the classic N-Queens problem.
A Super Queen is a chess piece that combines the movement patterns of a queen and a knight.

A Super Queen attacks:
- All squares in the same row (like a queen)
- All squares in the same column (like a queen)  
- All squares on both diagonals (like a queen)
- All squares reachable by a knight's L-shaped move

The goal is to count how many ways N Super Queens can be placed on an N×N chessboard
such that no two Super Queens attack each other.

The solution uses a BACKTRACKING algorithm that:
1. Tries placing a queen at a candidate position
2. Recursively solves for remaining positions
3. Backtracks by also trying NOT placing a queen at that position
4. Prunes invalid positions early for efficiency
-}
module SuperQueens
(countSuperQueens)
where

import Data.Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Sequence as Seq


-- | The side length of the square chessboard
type Side = Int

-- | X-coordinate on the board (1-indexed)
type X = Int

-- | Y-coordinate on the board (1-indexed)
type Y = Int

-- | A position on the chessboard with X and Y coordinates
data Position = Position X Y deriving (Show,Eq)

-- | A chessboard with a given side length and a sequence of Super Queen positions
data Board = Board Side (Seq Position) deriving (Show,Eq)

-- | Check if two Super Queens overlap horizontally (same row)
superQueensOverlapHorizontally (Position x1 _) (Position x2 _) = x1 == x2

-- | Check if two Super Queens overlap vertically (same column)
superQueensOverlapVertically (Position _ y1) (Position _ y2) = y1 == y2

-- | Check if two Super Queens overlap diagonally
superQueensOverlapDiagonally (Position x1 y1) (Position x2 y2) =
  let
    deltaX = x1 - x2
    deltaY = y1 - y2
  in abs deltaX == abs deltaY

-- | Check if two Super Queens overlap via knight's L-shaped move
-- A knight can attack 8 possible positions forming an L-shape
superQueenOverlapInL (Position x1 y1) pos2 =
 let possiblePositions =  [Position (x1 + 2) (y1 + 1),
                           Position (x1 + 2) (y1 - 1),
                           Position (x1 - 2) (y1 + 1),
                           Position (x1 - 2) (y1 - 1),
                           Position (x1 - 1) (y1 - 2),
                           Position (x1 - 1) (y1 + 2),
                           Position (x1 + 1) (y1 - 2),
                           Position (x1 + 1) (y1 + 2)]
 in elem pos2 possiblePositions

-- | Check if a new Super Queen can be placed on the board without attacking any existing queens
-- Returns True if the new queen doesn't attack any existing queens
canPlaceSuperQueen newSuperQueen (Board _ superQueens) =
  let
   superQueensOverlap currentSuperQueen = superQueensOverlapHorizontally currentSuperQueen newSuperQueen ||
                                          superQueensOverlapVertically currentSuperQueen newSuperQueen ||
                                          superQueensOverlapDiagonally currentSuperQueen newSuperQueen ||
                                          superQueenOverlapInL currentSuperQueen newSuperQueen
   discardOverlaps = not.any superQueensOverlap
  in discardOverlaps superQueens

-- | Generate all positions on a board of given size
-- Positions are generated in row-major order
allPositionsInBoard :: Int -> Seq Position
allPositionsInBoard size =
  let indexes = [1..size]
  in Seq.fromList $ [Position index1 index2 | index1 <- indexes, index2 <- indexes]

-- | Check if there are at least N rows remaining in the candidate positions
-- Used as a pruning optimization: if we need N more queens but have fewer than N rows left, we can't succeed
atLeastNRowsRemaining :: Int -> Seq Position -> Bool
atLeastNRowsRemaining numRows cells =
  let
    totalRows = Data.Set.size $ Prelude.foldl (flip Data.Set.insert) Data.Set.empty $ fmap (\ (Position x _) -> x) cells
  in totalRows >= numRows

-- | Find all valid board configurations for N Super Queens
-- 
-- This function implements a BACKTRACKING algorithm to explore all possible placements.
-- 
-- Backtracking Strategy:
-- For each candidate position, the algorithm explores TWO branches:
--   1. Place a queen at the current position and recursively solve for remaining positions
--   2. Skip the current position and try other positions (BACKTRACK)
-- The '++' operator concatenates solutions from both branches.
--
-- Pruning Optimizations:
-- - Removes positions that would attack already-placed queens
-- - Checks if enough rows remain to place remaining queens
-- - Ensures at least one queen per row for row-major order optimization
--
-- Returns a list of all valid board configurations
findAllBoardsForNQueens :: Board -> Seq Position -> [Board]
findAllBoardsForNQueens board@(Board size superQueens) Empty
  |  Seq.length superQueens == size = [board]
  | otherwise = []

findAllBoardsForNQueens (Board size superQueens) allRemaining@((Position x y) :<| _)
 | (y == 1) && (x > 1) && (not . List.any (\(Position prevX _) -> prevX == (x - 1)))  superQueens = []
 | not (atLeastNRowsRemaining (size - Seq.length superQueens) allRemaining) = []

findAllBoardsForNQueens board@(Board size superQueens) (nextPosition :<| remainingPositions)
 | Seq.length superQueens == size = [board]  -- Base case: found a valid solution
 | otherwise =
    let
       newBoard =  Board size (nextPosition <| superQueens)
       newPositions = prune newBoard remainingPositions
    -- BACKTRACKING: Try both placing and not placing a queen at nextPosition
    in findAllBoardsForNQueens newBoard newPositions       -- Branch 1: Place queen here
       ++ findAllBoardsForNQueens board remainingPositions  -- Branch 2: Skip this position (backtrack)

-- | Prune invalid positions from remaining candidates
-- Removes all positions that would attack any queen already on the board
prune board = Seq.filter (`canPlaceSuperQueen` board)

-- | Count the number of valid Super Queen placements for a board of given size
-- This is the main entry point for solving the Super Queens problem
countSuperQueens side = List.length $ findAllBoardsForNQueens (Board side Seq.empty) $ allPositionsInBoard side