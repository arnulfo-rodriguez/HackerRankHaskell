{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DefaultSignatures #-}

module Crosswords101 where

import Data.Map as Map
import Data.List as List
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Applicative
import Data.Sequence as Seq

data Point = Point {
  x :: Int,
  y :: Int
} deriving (Show,Ord)

type SparseGrid = Map Point [Vector]
data Vector = HorizontalVector {row :: Int, startCol :: Int, endCol :: Int} | VerticalVector {col :: Int, startRow :: Int, endRow :: Int } | SinglePoint {point :: Point} deriving (Show)
type EmptyWordSpace = Vector
data FilledWordSpace = FilledWordSpace { wordSpace :: Vector, word::[Char] } deriving (Show)
data FilledCell = FilledCell { filledCellLetter:: Char, filledCellPosition :: Int } deriving (Show)
data Constraints = Constraints {constraintLength :: Int, constraintFilledCells :: [FilledCell]} deriving (Show)
data CrosswordState = CrosswordState { pendingWords :: [[Char]], emptyWordSpaces :: [EmptyWordSpace], filledWordSpaces :: [FilledWordSpace] } deriving (Show)

vectorLength (HorizontalVector _ start end) = end - start + 1
vectorLength (VerticalVector _ start end) = end - start + 1

toFilledCell point@(Point x y) emptyWordSpace filledWordSpace = case wordSpace filledWordSpace of
                                        vv@VerticalVector {} ->  FilledCell (word filledWordSpace !! (x - startRow vv)) (y - startCol emptyWordSpace)
                                        hv@HorizontalVector {} -> FilledCell (word filledWordSpace !! (y - startCol hv)) (x - (startRow emptyWordSpace))
isSolved :: CrosswordState -> Bool
isSolved crosswordState = List.null $ emptyWordSpaces crosswordState

vectorIntersection :: EmptyWordSpace -> FilledWordSpace -> Maybe FilledCell
vectorIntersection v1 v2 = case (v1, wordSpace v2) of
  (HorizontalVector row startCol endCol , VerticalVector col startRow endRow) | row >= startRow && row <= endRow && col >= startCol && col <= endCol -> Just $ toFilledCell (Point row col) v1 v2
  (VerticalVector col startCol endCol , HorizontalVector row startRow endRow) | row >= startRow && row <= endRow && col >= startCol && col <= endCol -> Just $ toFilledCell (Point row col) v1 v2
  _ -> Nothing

findFilledCells :: EmptyWordSpace -> [FilledWordSpace] -> [FilledCell]
findFilledCells emptyWordSpaces = Data.Maybe.mapMaybe (vectorIntersection emptyWordSpaces)

getConstraintsForEmptyWordSpace emptyWordSpace filledWordSpaces = Constraints (vectorLength emptyWordSpace) (findFilledCells emptyWordSpace filledWordSpaces)
wordMatchesConstraint constraints w = List.length w == constraintLength constraints && all (\ fc -> w !! filledCellPosition fc == filledCellLetter fc ) (constraintFilledCells constraints)

trySolveCrosswords :: EmptyWordSpace -> [[Char]] -> [[Char]] -> [EmptyWordSpace] -> [FilledWordSpace] -> Maybe CrosswordState
trySolveCrosswords crosswordState [] _ _ _ = Nothing
trySolveCrosswords currentEmptyWordSpace (potentialMatchingWord:potentialMatchingWords) remainingWords emptyWordSpaces filledWordSpaces =
  solveCrosswords (CrosswordState (potentialMatchingWords ++ remainingWords) emptyWordSpaces (FilledWordSpace currentEmptyWordSpace potentialMatchingWord:filledWordSpaces)) <|>
  trySolveCrosswords currentEmptyWordSpace potentialMatchingWords (potentialMatchingWord:remainingWords) emptyWordSpaces filledWordSpaces

getRowEmptyPositions :: Int -> [Char] -> [Vector]
getRowEmptyPositions row values = List.map (\(_,index) -> SinglePoint (Point row index)) $ List.filter (\ (letter,_) -> letter == '-') $ List.zip values [0..]

getEmptyPositions :: [[Char]] -> [Point]
getEmptyPositions textPuzzle = List.zip [0..] textPuzzle >>= uncurry getRowEmptyPositions

isRightOf _ _ = False
isRightOf (SinglePoint (Point x1 y1)) (SinglePoint (Point x2 y2)) | x1 == x2 && y2 + 1 == y1 = True
isRightOf (SinglePoint (Point x1 y1)) (VerticalVector col startRow endRow) | y1 == col + 1 && (x1 >= startRow || x1 <= endRow) = True
isRightOf (SinglePoint (Point x1 y1)) (HorizontalVector row startCol endCol) | x1 == row && endCol + 1 == y1 = True

mergeRight (SinglePoint (Point x1 y1)) (SinglePoint (Point x2 y2)) | x1 == x2 && y2 + 1 == y1 = [HorizontalVector x1 y1 y2]
mergeRight (SinglePoint (Point x1 y1)) (HorizontalVector row startCol endCol) | x1 == row && endCol + 1 == y1 = (HorizontalVector row startCol y1)
mergeRight (SinglePoint (Point x1 y1)) vert@(VerticalVector col startRow endRow) | y1 == col + 1 && (x1 >= startRow || x1 <= endRow) = [vert,HorizontalVector x1 y2 y1]


isBelowOf _ _ = False
isBelowOf (SinglePoint (Point x1 y1)) (SinglePoint (Point x2 y2)) | y1 == y2 && x2 + 1 == x1 = True
isBelowOf (SinglePoint (Point x1 y1)) (VerticalVector col startRow endRow) | col == y1 && endRow + 1 == x1 = True
isBelowOf (SinglePoint (Point x1 y1)) (HorizontalVector row startCol endCol) | row + 1 == x1 && y1 >= startCol && y1 <= endCol = True

mergeBelow (SinglePoint (Point x1 y1)) (SinglePoint (Point x2 y2)) | y1 == y2 && x2 + 1 == x1 = [VerticalVector y1 x2 x1]
mergeBelow (SinglePoint (Point x1 y1)) (VerticalVector col startRow endRow) | col == y1 && endRow + 1 == x1 = [VerticalVector ]
mergeBelow (SinglePoint (Point x1 y1)) (HorizontalVector row startCol endCol) | row + 1 == x1 && y1 >= startCol && y1 <= endCol = True



mergeCurrent singlePoint@(SinglePoint _ ) [] = singlePoint
mergeCurrent singlePoint@(SinglePoint _ ) (head:rest) | isRightOf singlePoint head = mergeRight singlePoint head

mergeLeftToRightTopToBottom :: [Vector] -> [Vector]
mergeLeftToRightTopToBottom (current:rest) merged

buildEmptyWordSpaces :: [[Char]] -> [EmptyWordSpace]
buildEmptyWordSpaces textPuzzle =
  let
    emptyPositions = getRowEmptyPositions textPuzzle


solveCrosswords:: CrosswordState -> Maybe CrosswordState
solveCrosswords crosswordState | isSolved crosswordState = Just crosswordState
solveCrosswords (CrosswordState pendingWords (currentEmptyWordSpace:emptyWordSpaces) filledWordSpaces) =
  let
    constraints = getConstraintsForEmptyWordSpace currentEmptyWordSpace filledWordSpaces
    (potentialMatchingWords,remainingWords) = List.partition (wordMatchesConstraint constraints) pendingWords
  in trySolveCrosswords currentEmptyWordSpace potentialMatchingWords remainingWords emptyWordSpaces filledWordSpaces