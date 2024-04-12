{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Crosswords101 (solveCrosswordsMain) where

import Data.Map as Map
import Data.List as List
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Control.Applicative
import Data.Sequence as Seq
import Data.Foldable as Foldable
import Text.Printf (printf, PrintfArg)

data Point = Point {
  x :: Int,
  y :: Int
} deriving (Show, Eq, Ord)  

data Vector = HorizontalVector {row :: Int, startCol :: Int, endCol :: Int} | VerticalVector {col :: Int, startRow :: Int, endRow :: Int } deriving (Show)
type EmptyWordSpace = Vector
data FilledWordSpace = FilledWordSpace { wordSpace :: Vector, word::[Char] } deriving (Show)
data FilledCell = FilledCell { filledCellLetter:: Char, filledCellPosition :: Int } deriving (Show)
data Constraints = Constraints {constraintLength :: Int, constraintFilledCells :: [FilledCell]} deriving (Show)
data CrosswordState = CrosswordState { pendingWords :: [[Char]], remainingBlankSequences :: [EmptyWordSpace], filledWordSpaces :: [FilledWordSpace] } deriving (Show)

vectorLength (HorizontalVector _ start end) = end - start + 1
vectorLength (VerticalVector _ start end) = end - start + 1

toFilledCell :: Point -> EmptyWordSpace -> FilledWordSpace -> FilledCell
toFilledCell point@(Point x y) emptyWordSpace@HorizontalVector{} (FilledWordSpace vv@VerticalVector{} word) = FilledCell (word  !! (x - startRow vv)) (y - startCol emptyWordSpace)
toFilledCell point@(Point x y) emptyWordSpace@VerticalVector{} (FilledWordSpace hv@HorizontalVector{} word) = FilledCell (word !! (y - startCol hv)) (x - startRow emptyWordSpace)

isSolved :: CrosswordState -> Bool
isSolved crosswordState = List.null $ remainingBlankSequences crosswordState

vectorIntersection :: EmptyWordSpace -> FilledWordSpace -> Maybe FilledCell
vectorIntersection v1 v2 = case (v1, wordSpace v2) of
  (HorizontalVector row startCol endCol , VerticalVector col startRow endRow) | row >= startRow && row <= endRow && col >= startCol && col <= endCol -> Just $ toFilledCell (Point row col) v1 v2
  (VerticalVector col startRow endRow , HorizontalVector row startCol endCol) | row >= startRow && row <= endRow && col >= startCol && col <= endCol -> Just $ toFilledCell (Point row col) v1 v2
  _ -> Nothing

findFilledCells :: EmptyWordSpace -> [FilledWordSpace] -> [FilledCell]
findFilledCells emptyWordSpaces = Data.Maybe.mapMaybe (vectorIntersection emptyWordSpaces)

getConstraintsForEmptyWordSpace emptyWordSpace filledWordSpaces = Constraints (vectorLength emptyWordSpace) (findFilledCells emptyWordSpace filledWordSpaces)
wordMatchesConstraint constraints w = List.length w == constraintLength constraints && all (\ fc -> w !! filledCellPosition fc == filledCellLetter fc ) (constraintFilledCells constraints)

growTupleOrStartNew :: Int -> [(Int, Int, Int)] -> (Char, Int) -> [(Int, Int, Int)]
growTupleOrStartNew row ((_,initialCol,endCol):rest) (cr,currentCol) | cr == '-' && endCol + 1 == currentCol = (row,initialCol,currentCol):rest
growTupleOrStartNew row l (cr,col) | cr == '-' = (row,col,col):l
growTupleOrStartNew row l _ = l

getRowEmptyPositions :: Int -> [Char] -> [(Int,Int,Int)]
getRowEmptyPositions row values = List.foldl (growTupleOrStartNew row)  []  $ List.zip values [0..]

getEmptyRowPositions :: [[Char]] -> [(Int,Int,Int)]
getEmptyRowPositions textPuzzle = List.zip [0..] textPuzzle >>= uncurry getRowEmptyPositions

getHorizontalVectors :: [[Char]] -> [Vector]
getHorizontalVectors values =  List.map (\ (row,startCol,endCol) -> HorizontalVector row startCol endCol) $ List.filter (\ (_,x,y) -> y > x) $ getEmptyRowPositions values

-- Implement `getVerticalVectors` to traverse the grid vertically
getVerticalVectors :: [[Char]] -> [Vector]
getVerticalVectors values =
    List.map (\ (col, startRow, endRow) -> VerticalVector col startRow endRow) $ List.filter (\ (_,x,y) -> y > x) $ getEmptyRowPositions $ transposeGrid values

-- Implement `transposeGrid` to transpose the grid
transposeGrid :: [[Char]] -> [[Char]]
transposeGrid = List.transpose

buildEmptyCrosswordState:: [[Char]] -> [[Char]] -> CrosswordState
buildEmptyCrosswordState pendingWords values =
  let
    horizontalVectors = getHorizontalVectors values
    verticalVectors = getVerticalVectors values
    allVectors = horizontalVectors ++ verticalVectors
    startingState = CrosswordState pendingWords allVectors []
  in startingState

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

vectorToPoints :: Vector -> [Point]
vectorToPoints (HorizontalVector row startCol endCol) = List.map (Point row ) [startCol..endCol]
vectorToPoints (VerticalVector col startRow endRow) = List.map (`Point` col) [startRow..endRow]

filledWordSpaceToCells :: Map Point Char -> FilledWordSpace -> Map Point Char
filledWordSpaceToCells pointsMap (FilledWordSpace vector word) =
  List.foldl
    (\acc (currentChar,point) -> Map.insert point currentChar acc) pointsMap $ List.zip word $ vectorToPoints vector

filledWordSpacesToCells :: [FilledWordSpace] -> Map Point Char
filledWordSpacesToCells = List.foldl filledWordSpaceToCells Map.empty

renderRow :: Int -> Map Point Char -> [Char]
renderRow row points = List.map (\ currentCol -> Map.findWithDefault '+' (Point row currentCol) points)  [0..9]

renderFilledCrossword :: CrosswordState -> IO ()
renderFilledCrossword crosswordState =
    let
      filledWords = filledWordSpaces crosswordState
      filledPoints = filledWordSpacesToCells filledWords
    in
      do
        forM_ [0..9] $ \row -> do
          printf "%s\n" $ renderRow row filledPoints


solveCrosswordsMain :: IO()
solveCrosswordsMain = do
  lines <- replicateM 10 getLine
  wordsStr <- getLine
  let words = splitOn wordsStr ';'
  let result = solveCrosswords $ buildEmptyCrosswordState words $ Foldable.toList lines
  renderFilledCrossword $ Data.Maybe.fromJust result

solveCrosswords:: CrosswordState -> Maybe CrosswordState
solveCrosswords crosswordState | isSolved crosswordState = Just crosswordState
solveCrosswords (CrosswordState pendingWords (currentEmptyWordSpace:emptyWordSpaces) filledWordSpaces) =
  let
    constraints = getConstraintsForEmptyWordSpace currentEmptyWordSpace filledWordSpaces
    (potentialMatchingWords,remainingWords) = List.partition (wordMatchesConstraint constraints) pendingWords
  in trySolveCrosswords currentEmptyWordSpace potentialMatchingWords remainingWords emptyWordSpaces filledWordSpaces

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