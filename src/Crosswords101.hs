{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-|
Module      : Crosswords101
Description : Solution for the Crossword Puzzle Solver problem from HackerRank

== Problem Statement

Given a 10x10 crossword puzzle grid and a list of words, fill the puzzle such that:
- Each blank space (marked with '-') is filled with a letter
- Obstacles are marked with '+' and cannot be filled
- All provided words must be placed exactly once
- Words can be placed horizontally or vertically
- Words must fit into continuous sequences of blank spaces

Input:
- 10 lines representing the grid ('+' for obstacles, '-' for blanks)
- One line with semicolon-separated words to place

Output:
- The solved 10x10 grid with all words placed

== Algorithm Overview

This solution uses a backtracking algorithm with constraint propagation:

1. **Identify Word Spaces**: Find all continuous sequences of blanks (vectors)
   - Horizontal: left-to-right sequences in each row
   - Vertical: top-to-bottom sequences in each column (via transpose)

2. **Build Constraints**: For each empty vector, determine:
   - Required length (number of blanks)
   - Fixed letters (from intersecting filled vectors)

3. **Backtracking Search**: 
   - Pick an empty vector
   - Try each word that matches the constraints
   - Recursively solve remaining vectors
   - Backtrack if no solution found

4. **Constraint Checking**: A word matches if:
   - Length equals vector length
   - All intersection points have matching letters

The key insight is using intersection constraints to prune the search space
dramatically - each placed word constrains future placements through shared cells.

== Data Structures

- **Point**: 2D coordinate (x=row, y=col)
- **Vector**: Represents a continuous blank space
  - HorizontalVector: row and column range
  - VerticalVector: column and row range
- **FilledWordSpace**: A vector filled with a specific word
- **FilledCell**: Letter at a specific position (for constraints)
- **Constraints**: Length requirement + fixed letters for a vector
- **CrosswordState**: Current state of the puzzle
  - Pending words to place
  - Empty vectors remaining
  - Filled vectors so far

== Time Complexity

O(n! × m) in worst case where n = number of words, m = constraint checking time
In practice, constraint propagation makes it much faster (typically < 1 second)

-}
module Crosswords101 (solveCrosswordsMain) where

import Data.Map as Map
import Data.List as List
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Control.Applicative
import Data.Sequence as Seq
import Data.Foldable as Foldable
import Text.Printf (printf, PrintfArg)

-- | Represents a 2D coordinate in the grid
-- x = row index (0-9), y = column index (0-9)
data Point = Point {
  x :: Int,
  y :: Int
} deriving (Show, Eq, Ord)  

-- | Represents a continuous sequence of blank cells
-- HorizontalVector: blank cells in a row from startCol to endCol
-- VerticalVector: blank cells in a column from startRow to endRow
data Vector = HorizontalVector {row :: Int, startCol :: Int, endCol :: Int} | VerticalVector {col :: Int, startRow :: Int, endRow :: Int } deriving (Show)

-- | Type alias for an empty word space (to be filled)
type EmptyWordSpace = Vector

-- | Represents a vector that has been filled with a word
data FilledWordSpace = FilledWordSpace { wordSpace :: Vector, word::[Char] } deriving (Show)

-- | Represents a constraint on a single cell
-- filledCellLetter: the required letter at this position
-- filledCellPosition: the position within the vector (0-indexed)
data FilledCell = FilledCell { filledCellLetter:: Char, filledCellPosition :: Int } deriving (Show)

-- | Constraints for filling a vector with a word
-- constraintLength: required word length
-- constraintFilledCells: letters that must match at specific positions
data Constraints = Constraints {constraintLength :: Int, constraintFilledCells :: [FilledCell]} deriving (Show)

-- | Represents the current state of the puzzle solving process
-- pendingWords: words not yet placed
-- remainingBlankSequences: vectors not yet filled
-- filledWordSpaces: vectors that have been filled with words
data CrosswordState = CrosswordState { pendingWords :: [[Char]], remainingBlankSequences :: [EmptyWordSpace], filledWordSpaces :: [FilledWordSpace] } deriving (Show)

-- | Calculates the length of a vector (number of cells)
vectorLength (HorizontalVector _ start end) = end - start + 1
vectorLength (VerticalVector _ start end) = end - start + 1

-- | Converts an intersection point to a FilledCell constraint
-- Given an empty vector and a filled intersecting vector, determines
-- what letter must appear at the intersection and its position
toFilledCell :: Point -> EmptyWordSpace -> FilledWordSpace -> FilledCell
toFilledCell point@(Point x y) emptyWordSpace@HorizontalVector{} (FilledWordSpace vv@VerticalVector{} word) = FilledCell (word  !! (x - startRow vv)) (y - startCol emptyWordSpace)
toFilledCell point@(Point x y) emptyWordSpace@VerticalVector{} (FilledWordSpace hv@HorizontalVector{} word) = FilledCell (word !! (y - startCol hv)) (x - startRow emptyWordSpace)

-- | Checks if the puzzle is completely solved
-- A puzzle is solved when there are no remaining blank sequences
isSolved :: CrosswordState -> Bool
isSolved crosswordState = List.null $ remainingBlankSequences crosswordState

-- | Finds the intersection point between an empty vector and a filled vector
-- Returns Just FilledCell if they intersect, Nothing otherwise
-- Horizontal-Vertical or Vertical-Horizontal intersections only
vectorIntersection :: EmptyWordSpace -> FilledWordSpace -> Maybe FilledCell
vectorIntersection v1 v2 = case (v1, wordSpace v2) of
  (HorizontalVector row startCol endCol , VerticalVector col startRow endRow) | row >= startRow && row <= endRow && col >= startCol && col <= endCol -> Just $ toFilledCell (Point row col) v1 v2
  (VerticalVector col startRow endRow , HorizontalVector row startCol endCol) | row >= startRow && row <= endRow && col >= startCol && col <= endCol -> Just $ toFilledCell (Point row col) v1 v2
  _ -> Nothing

-- | Finds all filled cells that constrain an empty word space
-- Checks all filled word spaces for intersections with the empty space
findFilledCells :: EmptyWordSpace -> [FilledWordSpace] -> [FilledCell]
findFilledCells emptyWordSpaces = Data.Maybe.mapMaybe (vectorIntersection emptyWordSpaces)

-- | Builds constraints for an empty word space based on already-filled spaces
-- Returns length requirement and all intersection constraints
getConstraintsForEmptyWordSpace emptyWordSpace filledWordSpaces = Constraints (vectorLength emptyWordSpace) (findFilledCells emptyWordSpace filledWordSpaces)

-- | Checks if a word satisfies the given constraints
-- A word matches if: length is correct AND all fixed positions have correct letters
wordMatchesConstraint constraints w = List.length w == constraintLength constraints && all (\ fc -> w !! filledCellPosition fc == filledCellLetter fc ) (constraintFilledCells constraints)

-- | Accumulator function for finding continuous blank sequences in a row
-- If current char is '-' and extends existing sequence: grows it
-- If current char is '-' but doesn't extend: starts new sequence
-- If current char is not '-': returns list unchanged
-- Tuple format: (row, startCol, endCol)
growTupleOrStartNew :: Int -> [(Int, Int, Int)] -> (Char, Int) -> [(Int, Int, Int)]
growTupleOrStartNew row ((_,initialCol,endCol):rest) (cr,currentCol) | cr == '-' && endCol + 1 == currentCol = (row,initialCol,currentCol):rest
growTupleOrStartNew row l (cr,col) | cr == '-' = (row,col,col):l
growTupleOrStartNew row l _ = l

-- | Finds all continuous sequences of '-' characters in a single row
-- Returns list of (row, startCol, endCol) tuples
getRowEmptyPositions :: Int -> [Char] -> [(Int,Int,Int)]
getRowEmptyPositions row values = List.foldl (growTupleOrStartNew row)  []  $ List.zip values [0..]

-- | Finds all continuous blank sequences across all rows in the grid
getEmptyRowPositions :: [[Char]] -> [(Int,Int,Int)]
getEmptyRowPositions textPuzzle = List.zip [0..] textPuzzle >>= uncurry getRowEmptyPositions

-- | Extracts all horizontal word spaces from the grid
-- Filters out single-cell sequences (length must be > 1)
getHorizontalVectors :: [[Char]] -> [Vector]
getHorizontalVectors values =  List.map (\ (row,startCol,endCol) -> HorizontalVector row startCol endCol) $ List.filter (\ (_,x,y) -> y > x) $ getEmptyRowPositions values

-- | Extracts all vertical word spaces from the grid
-- Works by transposing the grid and finding "horizontal" sequences
-- Filters out single-cell sequences (length must be > 1)
getVerticalVectors :: [[Char]] -> [Vector]
getVerticalVectors values =
    List.map (\ (col, startRow, endRow) -> VerticalVector col startRow endRow) $ List.filter (\ (_,x,y) -> y > x) $ getEmptyRowPositions $ transposeGrid values

-- | Transposes a 2D grid (converts rows to columns)
-- Used to find vertical vectors using the same algorithm as horizontal
transposeGrid :: [[Char]] -> [[Char]]
transposeGrid = List.transpose

-- | Initializes the crossword state from a grid and word list
-- Finds all word spaces (horizontal and vertical) and creates initial state
buildEmptyCrosswordState:: [[Char]] -> [[Char]] -> CrosswordState
buildEmptyCrosswordState pendingWords values =
  let
    horizontalVectors = getHorizontalVectors values
    verticalVectors = getVerticalVectors values
    allVectors = horizontalVectors ++ verticalVectors
    startingState = CrosswordState pendingWords allVectors []
  in startingState

-- | Splits a string on a separator character
-- Example: splitOn "a;b;c" ';' = ["a","b","c"]
splitOn :: [Char] -> Char -> [[Char]]
splitOn str separator =
  List.foldl
    (\ acc current ->
                     case acc of
                       [] | current == separator -> []
                       [] -> [[current]]
                       l@(_:_) | current == separator -> "":l
                       (head:tail) -> (current:head):tail
    )  [] (List.reverse str)

-- | Converts a vector to a list of all points it contains
-- Used for mapping words to grid positions
vectorToPoints :: Vector -> [Point]
vectorToPoints (HorizontalVector row startCol endCol) = List.map (Point row ) [startCol..endCol]
vectorToPoints (VerticalVector col startRow endRow) = List.map (`Point` col) [startRow..endRow]

-- | Adds all cells from a filled word space to the point map
-- Maps each letter of the word to its corresponding grid position
filledWordSpaceToCells :: Map Point Char -> FilledWordSpace -> Map Point Char
filledWordSpaceToCells pointsMap (FilledWordSpace vector word) =
  List.foldl
    (\acc (currentChar,point) -> Map.insert point currentChar acc) pointsMap $ List.zip word $ vectorToPoints vector

-- | Converts all filled word spaces to a map of (Point -> Char)
-- Used for rendering the final grid
filledWordSpacesToCells :: [FilledWordSpace] -> Map Point Char
filledWordSpacesToCells = List.foldl filledWordSpaceToCells Map.empty

-- | Renders a single row of the crossword grid
-- For each column, looks up the character in the map or uses '+' as default
renderRow :: Int -> Map Point Char -> [Char]
renderRow row points = List.map (\ currentCol -> Map.findWithDefault '+' (Point row currentCol) points)  [0..9]

-- | Renders and prints the complete solved crossword grid
-- Converts filled word spaces to a point map and prints each row
renderFilledCrossword :: CrosswordState -> IO ()
renderFilledCrossword crosswordState =
    let
      filledWords = filledWordSpaces crosswordState
      filledPoints = filledWordSpacesToCells filledWords
    in
      do
        forM_ [0..9] $ \row -> do
          printf "%s\n" $ renderRow row filledPoints

-- | Main entry point for the crossword solver
-- Reads input, solves puzzle, and prints result
solveCrosswordsMain :: IO()
solveCrosswordsMain = do
  lines <- replicateM 10 getLine
  wordsStr <- getLine
  let words = splitOn wordsStr ';'
  let result = solveCrosswords $ buildEmptyCrosswordState words $ Foldable.toList lines
  renderFilledCrossword $ Data.Maybe.fromJust result

-- | Main backtracking solver function
-- Returns Just CrosswordState if solved, Nothing if no solution exists
-- Base case: if solved, return current state
-- Recursive case: try placing each matching word and recurse
solveCrosswords:: CrosswordState -> Maybe CrosswordState
solveCrosswords crosswordState | isSolved crosswordState = Just crosswordState
solveCrosswords (CrosswordState pendingWords (currentEmptyWordSpace:emptyWordSpaces) filledWordSpaces) =
  let
    constraints = getConstraintsForEmptyWordSpace currentEmptyWordSpace filledWordSpaces
    (potentialMatchingWords,remainingWords) = List.partition (wordMatchesConstraint constraints) pendingWords
  in trySolveCrosswords currentEmptyWordSpace potentialMatchingWords remainingWords emptyWordSpaces filledWordSpaces

-- | Tries each potential word for a given empty space
-- Uses backtracking: if a word doesn't lead to a solution, try the next one
-- Uses Alternative (<|>) to chain attempts
-- Returns Nothing if no word works
trySolveCrosswords :: EmptyWordSpace -> [[Char]] -> [[Char]] -> [EmptyWordSpace] -> [FilledWordSpace] -> Maybe CrosswordState
trySolveCrosswords crosswordState [] _ _ _ = Nothing
trySolveCrosswords currentEmptyWordSpace (potentialMatchingWord:potentialMatchingWords) remainingWords emptyWordSpaces filledWordSpaces =
  let
    newRemainingWordsSet = (potentialMatchingWords ++ remainingWords)
    nextStateToTry = CrosswordState newRemainingWordsSet  emptyWordSpaces (FilledWordSpace currentEmptyWordSpace potentialMatchingWord:filledWordSpaces)
  in
    solveCrosswords nextStateToTry <|> trySolveCrosswords currentEmptyWordSpace potentialMatchingWords (potentialMatchingWord:remainingWords) emptyWordSpaces filledWordSpaces

main :: IO()
main = solveCrosswordsMain