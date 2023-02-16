{-# LANGUAGE TupleSections #-}

module SuperQueens
(countSuperQueens)
where

import Data.Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Sequence as Seq


type Side = Int
type X = Int
type Y = Int
data Position = Position X Y deriving (Show,Eq)
data Board = Board Side (Seq Position) deriving (Show,Eq)

superQueensOverlapHorizontally (Position x1 _) (Position x2 _) = x1 == x2
superQueensOverlapVertically (Position _ y1) (Position _ y2) = y1 == y2
superQueensOverlapDiagonally (Position x1 y1) (Position x2 y2) =
  let
    deltaX = x1 - x2
    deltaY = y1 - y2
  in abs deltaX == abs deltaY
superQueenOverlapInL (Position x1 y1) pos2 =
 let possiblePositions =  [Position (x1 + 2) (y1 + 1),
                           Position (x1 + 2) (y1 - 1),
                           Position (x1 - 2) (y1 + 1),
                           Position (x1 - 2) (y1 - 1),
                           Position (x1 - 1) (y1 - 2),
                           Position (x1 - 1) (y1 + 2),
                           Position (x1 + 1) (y1 - 2),
                           Position (x1 + 1) (y1 + 2)]
 in (Maybe.isJust . List.find (== pos2)) possiblePositions

canPlaceSuperQueen newSuperQueen (Board _ superQueens) =
  let
   superQueensOverlap currentSuperQueen = superQueensOverlapHorizontally currentSuperQueen newSuperQueen ||
                                          superQueensOverlapVertically currentSuperQueen newSuperQueen ||
                                          superQueensOverlapDiagonally currentSuperQueen newSuperQueen ||
                                          superQueenOverlapInL currentSuperQueen newSuperQueen
   discardOverlaps = not.any superQueensOverlap
  in discardOverlaps superQueens

allPositionsInBoard :: Int -> (Seq Position)
allPositionsInBoard size =
  let indexes = [1..size]
  in Seq.fromList $ [Position index1 index2 | index1 <- indexes, index2 <- indexes]

atLeastNRowsRemaining :: Int -> Seq Position -> Bool
atLeastNRowsRemaining numRows cells =
  let
    totalRows = Data.Set.size $ Prelude.foldl (flip Data.Set.insert) Data.Set.empty $ fmap (\ (Position x _) -> x) cells
  in totalRows >= numRows

findAllBoardsForNQueens :: Board -> (Seq Position) -> [Board]
findAllBoardsForNQueens board@(Board size superQueens) Empty
  |  Seq.length superQueens == size = [board]
  | otherwise = []

findAllBoardsForNQueens (Board size superQueens) allRemaining@((Position x y) :<| _)
 | (y == 1) && (x > 1) && (not . List.any (\(Position prevX _) -> prevX == (x - 1)))  superQueens = []
 | not (atLeastNRowsRemaining (size - Seq.length superQueens) allRemaining) = []

findAllBoardsForNQueens board@(Board size superQueens) (nextPosition :<| remainingPositions)
 | Seq.length superQueens == size = [board]
 | otherwise =
    let
       newBoard =  Board size (nextPosition <| superQueens)
       newPositions = prune newBoard remainingPositions
    in findAllBoardsForNQueens newBoard newPositions ++ findAllBoardsForNQueens board remainingPositions

prune board = Seq.filter (`canPlaceSuperQueen` board)

countSuperQueens side = List.length $ findAllBoardsForNQueens (Board side Seq.empty) $ allPositionsInBoard side