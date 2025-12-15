{-|
Module      : MagicSquare
Description : Solution for the Magic Square problem from HackerRank

== Problem Statement

Given a 3×3 grid of integers, determine the minimum cost to convert it into a 
magic square. The cost is the sum of absolute differences between the original 
and target values.

A magic square is a 3×3 grid where:
- All rows sum to the same value (the magic constant)
- All columns sum to the same value
- Both diagonals sum to the same value
- For a 3×3 magic square with digits 1-9, the magic constant is always 15

Example:
```
4 9 2
3 5 7
8 1 6
```
All rows, columns, and diagonals sum to 15.

== Algorithm Overview

**Key Insight:** There are exactly 8 distinct 3×3 magic squares using digits 1-9.
These are:
1. One fundamental magic square
2. Its 3 rotations (90°, 180°, 270°)
3. Its reflection
4. The reflection's 3 rotations

Instead of searching for a solution, we:
1. Pre-compute or hardcode all 8 magic squares
2. Calculate the "distance" (sum of absolute differences) to each
3. Return the minimum distance

**Why this works:**
- 3×3 magic squares with digits 1-9 form a very small finite set
- Brute force checking all 9! = 362,880 permutations is feasible
- Filtering to magic squares gives exactly 8 results
- Computing distance to all 8 is O(72) = O(1) - constant time!

== Mathematical Background

For a 3×3 magic square using 1-9:
- Sum of all numbers: 1+2+...+9 = 45
- Number of rows: 3
- Magic constant: 45/3 = 15

The center must always be 5 (can be proven mathematically).

== Implementation Approach

This module provides two approaches:

1. **Dynamic Generation** (allMagicSquares):
   - Generates all permutations of [1..9]
   - Filters to magic squares
   - Useful for verification but slower

2. **Hardcoded List** (fixedMagicSquares):
   - Pre-computed list of all 8 magic squares
   - Used in formingMagicSquare for O(1) solution
   - Much faster for solving the problem

== Time Complexity

- Dynamic generation: O(n!) where n=9, so O(362,880) to generate all
- Actual solution (formingMagicSquare): O(1) - comparing to 8 fixed squares
- Distance calculation per square: O(9) - comparing 9 cells

-}
module MagicSquare 
(allMagicSquares) where

-- | Represents a 2D matrix of integers
newtype Matrix = Matrix [[Int]]
 deriving (Show)

-- | Represents a position in the matrix (row, col)
data Pos = Pos Int Int

-- | Represents a move/swap between two positions (unused in current solution)
data Move = Move (Pos,Pos) Int Matrix

-- | Checks if all elements in a list are equal
-- Returns Just value if all equal, Nothing if any differ
-- Used to verify magic square properties
allEqual [] = Nothing
allEqual [f] = Just f
allEqual [f, s] = if f == s then Just f else Nothing
allEqual (f : s : rest) = if f == s then allEqual (s : rest) else Nothing

-- | Helper to check if all sides (0, 1, 2) satisfy a property equally
-- Applies function f to indices 0, 1, 2 and checks if all results are equal
allEqualForSides f = allEqual $ map f [0 .. 2]

-- | Returns the side length of the matrix (number of rows)
side (Matrix arr) = length arr

-- | Calculates the sum of a specific row
sumRow (Matrix arr) row = sum (arr !! row)

-- | Checks if all rows have equal sums
-- Returns Just sum if all rows equal, Nothing otherwise
allRowsEqual m = allEqualForSides $ sumRow m

-- | Calculates the sum of a specific column
sumCol (Matrix arr) col = foldl (\acc current -> acc + (current !! col)) 0 arr

-- | Checks if all columns have equal sums
-- Returns Just sum if all columns equal, Nothing otherwise
allColsEqual m = allEqualForSides $ sumCol m

-- | Calculates the sum of the main diagonal (top-left to bottom-right)
-- Sums elements where row index equals column index
sumDiag1 (Matrix arr) = foldl (\acc current -> acc + (arr !! current !! current)) 0 [0 .. (length arr - 1)]

-- | Calculates the sum of the anti-diagonal (top-right to bottom-left)
-- Sums elements where row + column = n-1
sumDiag2 (Matrix arr) =
  let last = length arr - 1
   in foldl (\acc current -> acc + (arr !! current !! (last - current))) 0 [0 .. last]

-- | Checks if a matrix is a magic square
-- Returns True if:
--   1. All rows sum to the same value
--   2. All columns sum to the same value
--   3. Both diagonals equal the row/column sum
-- For 3×3 with digits 1-9, this sum should be 15
isMagicSquare matrix =
  let allRowsEq = allRowsEqual matrix
      allColsEq = allColsEqual matrix
      d1Sum = sumDiag1 matrix
      d2Sum = sumDiag2 matrix
   in case (allRowsEq, allColsEq) of
        (Just x, Just y) -> case allEqual [x , y ,d1Sum ,d2Sum] of
                                    Just _ -> True
                                    Nothing -> False
        _ -> False

-- | Inserts an element into all possible positions of a list
-- Returns a list of all possible insertions
-- Example: addToAllPositions [] 'x' "ab" = ["xab", "axb", "abx"]
-- Used as a building block for generating permutations
addToAllPositions [] e [] = [[e]]
addToAllPositions prev e [] = [prev ++ [e]]
addToAllPositions prev e arr@(head:tail) = (prev ++ (e:arr)) : addToAllPositions (prev ++ [head]) e tail

-- | Generates all permutations of a list
-- For [1,2,3,...,9], this generates all 9! = 362,880 permutations
-- Each permutation represents a potential magic square arrangement
allPermutations :: [e] -> [[e]]
allPermutations  = foldl (\result current ->
                             case result of
                               [] -> [[current]] 
                               _ -> concatMap (addToAllPositions [] current) result
                        ) [] 

-- | Splits a list into chunks of size n
-- Example: splitEvery 3 [1,2,3,4,5,6] = [[1,2,3],[4,5,6]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

-- | Converts a flat list of 9 elements into a 3×3 Matrix
-- Example: [1,2,3,4,5,6,7,8,9] becomes [[1,2,3],[4,5,6],[7,8,9]]
toMatrix arr = Matrix $ splitEvery 3 arr

-- | Generates all 8 magic squares by brute force
-- Process:
--   1. Generate all 362,880 permutations of [1..9]
--   2. Convert each to a 3×3 matrix
--   3. Filter to only magic squares (exactly 8 remain)
-- Note: This is computationally expensive; fixedMagicSquares is preferred
allMagicSquares = filter isMagicSquare $ map toMatrix $ allPermutations [1,2,3,4,5,6,7,8,9] 

-- | Pre-computed list of all 8 possible 3×3 magic squares using digits 1-9
-- These are the only valid magic squares and include:
--   - 1 fundamental square
--   - 3 rotations of it (90°, 180°, 270°)
--   - 1 reflection
--   - 3 rotations of the reflection
-- All squares have magic constant 15 (sum of each row/column/diagonal)
fixedMagicSquares = [Matrix [[4,9,2],
                             [3,5,7],
                             [8,1,6]],
                     Matrix [[2,9,4],
                             [7,5,3],
                             [6,1,8]],
                     Matrix [[2,7,6],
                             [9,5,1],
                             [4,3,8]],
                     Matrix [[6,7,2],
                             [1,5,9],
                             [8,3,4]],
                     Matrix [[4,3,8],
                             [9,5,1],
                             [2,7,6]],
                     Matrix [[8,3,4],
                             [1,5,9],
                             [6,7,2]],
                     Matrix [[8,1,6],
                             [3,5,7],
                             [4,9,2]],
                     Matrix [[6,1,8],
                             [7,5,3],
                             [2,9,4]]]

-- | Calculates the distance between two matrices
-- Distance is the sum of absolute differences of corresponding elements
-- Example: distance [[1,2,3],[4,5,6],[7,8,9]] [[1,2,5],[4,5,6],[7,8,9]] = |3-5| = 2
-- This represents the minimum cost to transform one matrix into the other
distance (Matrix m) (Matrix other) =  foldl (\ d (a,b) -> d + abs(a - b)) 0  $ concatMap (uncurry zip) $  zip m other

-- | Main solution: Finds minimum cost to convert a matrix into a magic square
-- Algorithm:
--   1. Calculate distance to each of the 8 pre-computed magic squares
--   2. Return the minimum distance
-- Time: O(8 × 9) = O(1) constant time
-- This is optimal since we only need to check 8 fixed possibilities
formingMagicSquare s = minimum $ map (distance (Matrix s)) fixedMagicSquares