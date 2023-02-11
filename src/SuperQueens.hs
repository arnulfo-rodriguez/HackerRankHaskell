{-# LANGUAGE TupleSections #-}

module SuperQueens where

import Data.Map
import Data.Set
import Data.List as List
import Data.Maybe
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
 in (isJust . List.find (== pos2)) possiblePositions
 
canPlaceSuperQueen newSuperQueen (Board _ superQueens) =
  let 
   superQueensOverlap currentSuperQueen = superQueensOverlapHorizontally currentSuperQueen newSuperQueen ||
                                          superQueensOverlapVertically currentSuperQueen newSuperQueen ||
                                          superQueensOverlapDiagonally currentSuperQueen newSuperQueen ||
                                          superQueenOverlapInL currentSuperQueen newSuperQueen
   checkAllPlacedQueens Seq.Empty = True
   checkAllPlacedQueens (currentSuperQueen :<| remainingSuperQueens) = not(superQueensOverlap currentSuperQueen) && checkAllPlacedQueens remainingSuperQueens
  in checkAllPlacedQueens superQueens

allPositionsInBoard :: Int -> [Position]
allPositionsInBoard size =
  let indexes = [1..size]
  in List.concatMap (\ index1 -> List.map (Position index1) indexes)  indexes
  
placeNSuperQueens :: Board -> [Position] -> [Board]
placeNSuperQueens board@(Board size superQueens) [] 
  |  Seq.length superQueens == size = [board]
  | otherwise = []
  
placeNSuperQueens board@(Board size superQueens) (nextPosition:remainingPositions)
 | Seq.length superQueens == size = [board]
 | Seq.length superQueens < size && canPlaceSuperQueen nextPosition board =
    let
       newBoard =  Board size (nextPosition <| superQueens)
    in placeNSuperQueens newBoard remainingPositions ++ placeNSuperQueens board remainingPositions
 | otherwise = placeNSuperQueens board remainingPositions
 
countSuperQueens side = List.length $ placeNSuperQueens (Board side Seq.empty) $ allPositionsInBoard side