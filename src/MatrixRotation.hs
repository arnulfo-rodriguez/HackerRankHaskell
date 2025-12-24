{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}


module MatrixRotation
(matrixRotation, showMatrix)
where

import Data.Sequence as Seq
import Data.List as List
import Matrix
import Data.Ratio as Ratio
import Text.Printf as Printf
import Control.Monad as Monad

-- | Generate the indexes of the outer layer of a matrix in counterclockwise order.
-- Given dimensions n (rows) and m (columns), this creates a sequence of (row, col) tuples
-- starting from top-left and moving clockwise: top row -> right column -> bottom row -> left column.
-- This is used to extract the "ring" of elements that need to be rotated together.
outerLayerIndexes n m =
  let
    -- Top row: (0,0), (0,1), ..., (0,m-1)
    topRow = List.map (0,) [0..(m-1)]
    -- Left column (excluding corners): (n-2,0), (n-3,0), ..., (1,0)
    -- Going bottom to top (reverse order)
    leftCol =   List.map (, 0) [(n-2),(n - 3)..1] 
    -- Bottom row: (n-1,m-1), (n-1,m-2), ..., (n-1,0)
    -- Going right to left (reverse order)
    bottomRow = List.map (n - 1,) [(m-1),(m - 2)..0]
    -- Right column (excluding corners): (1,m-1), (2,m-1), ..., (n-2,m-1)
    -- Going top to bottom
    rightCol = List.map (, m - 1) [1..(n-2)]
  in Seq.fromList $ topRow ++ rightCol ++ bottomRow ++ leftCol

-- | Adjust layer coordinates to real matrix positions.
-- When processing nested layers (rings) of a matrix, each inner layer has coordinates
-- relative to its own frame. This function adds the layer offset to convert them to
-- absolute matrix positions. For example, layer 1 means we add 1 to both x and y.
mapIndexesToRealPosition layer = Seq.mapWithIndex (\ _ (x,y) -> (x + layer, y + layer))

-- | Rotate a sequence to the left by a given number of turns.
-- This simulates rotating the elements in a ring. Uses modulo to handle cases where
-- turns exceeds the sequence length (full rotations don't change the result).
-- Splits the sequence at the rotation point and concatenates them in reverse order.
-- Example: shiftLeft 2 [1,2,3,4,5] = [3,4,5,1,2]
shiftLeft turns s =
  let realTurns = turns `mod` Seq.length s  -- Handle full rotations efficiently
      (first,second) = Seq.splitAt realTurns s  -- Split at rotation point
  in second >< first  -- Concatenate: second part + first part

-- | Convert a flat sequence back into the four sides of a matrix layer.
-- Takes a sequence representing a rotated ring and splits it back into:
-- (topRow, leftCol, bottomRow, rightCol) tuples.
-- The left and bottom sequences are reversed because they were originally stored
-- in reverse order (to maintain counterclockwise traversal).
toRowsAndCols seq n m =
  let
    -- First m elements are the top row
    (topRow,rem1) =  Seq.splitAt m seq
    -- Next (n-2) elements are the right column (excluding corners)
    (rightCol,rem2) = Seq.splitAt (n - 2) rem1
    -- Next m elements are the bottom row, remaining are left column
    (bottomRow,leftCol) = Seq.splitAt m rem2
  in (topRow,Seq.reverse leftCol,Seq.reverse bottomRow,rightCol)

-- | Reconstruct a single row from the nested layer representation.
-- Takes a sequence of layer tuples (topRow, leftCol, bottomRow, rightCol) and a row index n.
-- Recursively builds the row by combining elements from the appropriate layer sides.
-- Row 0 is the top row of the outermost layer.
-- Middle rows are built from left column elements, inner layers, and right column elements.
-- The last row comes from the bottom row of the outermost layer.
getMatrixRow :: Seq (Seq Rational,Seq Rational,Seq Rational,Seq Rational) -> Int -> Seq Rational
getMatrixRow Empty _ = Empty  -- No layers, return empty
getMatrixRow  ((topRow,_,_,_) :<| _) 0 = topRow  -- Row 0 is the top row
getMatrixRow  ((_,leftCol,bottomRow,rightCol) :<| rest) n
 -- Middle rows: left edge + inner layers + right edge
 | n <= Seq.length leftCol = ((leftCol `Seq.index` (n - 1)) <| getMatrixRow rest (n - 1)) |> (rightCol `Seq.index` (n - 1))
 -- Last row is the bottom row
 | otherwise = bottomRow

-- | Extract cell values from a matrix given a sequence of coordinate pairs.
-- Maps each (row, col) pair to the actual value stored at that position in the matrix.
-- This is used to get the values from a layer's ring of positions.
toCellValues:: Matrix ->  Seq (Int,Int) -> Seq Rational
toCellValues m = Seq.mapWithIndex (\ _ (x, y) -> getCell x y m)

-- | Display a matrix to stdout in a formatted grid.
-- Each element is printed as an integer (using the numerator since values are Rational),
-- with rows separated by newlines. This is the output format expected by HackerRank.
showMatrix (Matrix sq) =
  let
      showRow :: Seq Rational -> IO (Seq ())
      showRow row = forM row (\ number -> do
                                            -- Extract integer part (numerator) and print with space
                                            putStr $ printf "%d " (Ratio.numerator number))
  in forM sq (\ row -> 
                   do 
                     _ <- showRow row
                     putStrLn ""  -- New line after each row
                     )

-- | Rotate a matrix counterclockwise by r positions.
-- This is the main function that solves the matrix rotation problem.
-- Algorithm:
--   1. Identify all nested layers (rings) in the matrix
--   2. For each layer, extract the ring of values in counterclockwise order
--   3. Rotate each ring to the left by r positions
--   4. Convert rotated rings back to layer representation (4 sides)
--   5. Reconstruct the matrix row by row from all rotated layers
matrixRotation :: Matrix -> Int -> Matrix
matrixRotation m r =
  let
    (rowCount,colCount) = getDimensions m
    -- A 4x6 matrix has 2 nested layers, 5x5 has 2, etc.
    numberOfNestedMatrices = rowCount `div` 2
    -- Generate coordinate sequences for each layer (0=outer, 1=next inner, etc.)
    listsOfIndexes :: [Seq(Int,Int)]
    listsOfIndexes = List.map (\ i ->  mapIndexesToRealPosition i (outerLayerIndexes (rowCount - (2*i)) (colCount - (2*i))))  [0 .. (numberOfNestedMatrices - 1)]
    -- Extract actual values from the matrix at those coordinates
    innerLists =  List.map (toCellValues m) listsOfIndexes
    -- Rotate each ring left by r positions
    rotatedLists = Seq.fromList $ List.map (shiftLeft r) innerLists
    -- Split each rotated ring back into (top, left, bottom, right) sides
    rowsAndCols = Seq.mapWithIndex (\ i s -> toRowsAndCols s (rowCount - (2*i)) (colCount - (2*i))) rotatedLists
    -- Reconstruct each row from the layer representations
    newRows = Seq.fromList $ List.map (getMatrixRow rowsAndCols)  [0..rowCount -1]
  in Matrix newRows