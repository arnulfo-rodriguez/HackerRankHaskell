module Matrix
  ( Matrix (..),
    subtractMatrix,
    multiply,
    getRow,
    buildIdentityMatrix,
    inverseGaussJordan,
    getCell,
    subMatrix,
    getDimensions
  )
where

import Data.Foldable ( Foldable(sum) )
import Data.Function as Function ( on )
import Data.List ( map, foldl )
import Data.Sequence as Seq
    ( (><),
      drop,
      foldlWithIndex,
      fromList,
      index,
      length,
      mapWithIndex,
      splitAt,
      take,
      zipWith,
      (|>),
      sortBy,
      Seq((:<|), Empty) )
newtype Matrix = Matrix (Seq (Seq Rational)) deriving (Show)

data IndexAndValueLeftMostNonZero = BiggestLeftMostTuple Int Rational deriving (Eq,Show)

-- | Custom Ord instance for sorting rows during Gauss-Jordan elimination
-- Rows are first compared by the column index of their leftmost non-zero element
-- If indices are equal, rows with larger values come first (secondary sort by value descending)
instance Ord IndexAndValueLeftMostNonZero where
  (BiggestLeftMostTuple index1 value1) `compare` (BiggestLeftMostTuple index2 value2) = case index1 `compare` index2 of
    EQ -> value2 `compare` value1  -- If same column, prefer larger value
    comp -> comp

-- | Generic element-wise binary operation on two matrices
-- Applies the given operation to corresponding elements in both matrices
-- The inner zipWith applies the operation to elements, outer zipWith processes rows
genericAdd :: (Rational -> Rational -> Rational) -> Matrix -> Matrix -> Matrix
genericAdd op (Matrix seq1) (Matrix seq2) = Matrix $ Seq.zipWith (Seq.zipWith op) seq1 seq2

-- | Element-wise matrix addition
add :: Matrix -> Matrix -> Matrix
add = genericAdd (+)

-- | Element-wise matrix subtraction
subtractMatrix :: Matrix -> Matrix -> Matrix
subtractMatrix = genericAdd (-)

-- | Extract column i from a matrix
-- Returns a sequence containing the i-th element from each row
getColumn :: Int -> Matrix -> Seq Rational
getColumn i (Matrix seq) = Seq.mapWithIndex (\_ v -> v `Seq.index` i) seq

-- | Extract row i from a matrix
-- Returns the entire row as a sequence
getRow :: Int -> Matrix -> Seq Rational
getRow i (Matrix seq) = seq `Seq.index` i

-- | Get the cell value at position (i, j) in the matrix
-- i is the row index, j is the column index
getCell :: Int -> Int -> Matrix -> Rational
getCell i j m = getRow i m `Seq.index` j

-- | Get the dimensions of a matrix as (rows, columns)
-- Returns (0, 0) for an empty matrix
getDimensions :: Matrix -> (Int, Int)
getDimensions m@(Matrix seq) = let rowsCount = Seq.length seq
                                   colsCount =  if rowsCount > 0 then Seq.length (getRow 0 m) else 0
                                in (rowsCount,colsCount)

-- | Matrix multiplication (standard linear algebra multiplication)
-- For matrices A (m×n) and B (n×p), produces matrix C (m×p)
-- where C[i,j] = sum of A[i,k] * B[k,j] for all k
multiply :: Matrix -> Matrix -> Matrix
multiply (Matrix seq1) m2@(Matrix seq2) =
  let -- Compute a single cell by dot product of row and column
      getCellMultiplication row column = Data.Foldable.sum $ Seq.zipWith (*) row column
      -- Compute entire row by multiplying current row with each column of m2
      getRowMultiplication currentRow = Seq.fromList $ Data.List.map (\i -> getCellMultiplication currentRow (getColumn i m2)) [0 .. Seq.length (seq2 `Seq.index` 0) -1]
   in Matrix $ Seq.mapWithIndex (\_ currentRow -> getRowMultiplication currentRow) seq1

-- | Divide all elements of a matrix by a scalar value
divideBy :: Matrix -> Rational -> Matrix
divideBy (Matrix seq) n = Matrix $ Seq.mapWithIndex (\_ row -> Seq.mapWithIndex (\_ entry -> entry / n) row) seq

-- | Extract a sub-matrix from the given matrix
-- Takes rows from startX (inclusive) to endX (exclusive)
-- Takes columns from startY (inclusive) to endY (exclusive)
-- Uses drop to skip initial rows/columns, then take to get the desired range
subMatrix :: Int -> Int -> Int -> Int -> Matrix -> Matrix
subMatrix startX startY endX endY (Matrix seq) =  Matrix $ Seq.mapWithIndex  (\ _ row -> Seq.take (endY - startY) (Seq.drop startY row))
                                                                             (Seq.take (endX - startX) (Seq.drop startX seq))

-- | Transpose a matrix (swap rows and columns)
-- Converts each column into a row in the result
transpose_ :: Matrix -> Matrix
transpose_ m@(Matrix seq) = Matrix (Seq.fromList (Data.List.map (`getColumn` m) [0 .. Seq.length seq -1]))

-- | Build an identity matrix with the same dimensions as the given matrix
-- An identity matrix has 1s on the main diagonal and 0s elsewhere
-- Used in the Gauss-Jordan elimination process
buildIdentityMatrix :: Matrix -> Matrix
buildIdentityMatrix (Matrix seq) = Matrix $ Seq.mapWithIndex (\i row -> Seq.mapWithIndex (\j _ -> if i == j then 1 else 0) row) seq

-- | Check if a matrix is an identity matrix
-- Returns True if all diagonal elements are 1 and all off-diagonal elements are 0
isIdentityMatrix :: Matrix -> Bool
isIdentityMatrix (Matrix seq) =
  let
    -- Check if row j has 1 at position j and 0s elsewhere
    isIdentityRow j = Seq.foldlWithIndex (\ acc index value -> acc && (value == (if index == j then 1 else 0))) True
  in Seq.foldlWithIndex (\ acc index row -> acc && isIdentityRow index row) True seq

-- | Horizontally concatenate two matrices (side by side)
-- Used in Gauss-Jordan to create augmented matrix [A|I] for finding inverse
-- The operator (><) concatenates two sequences
concatenate :: Matrix -> Matrix -> Seq (Seq Rational)
concatenate (Matrix seq1) (Matrix seq2) = Seq.zipWith (><) seq1 seq2

-- | Check if the first 'max' elements of a sequence are all zeros
-- Used to determine if a row is all zeros during elimination
isAllZeroes :: Int -> Seq Rational -> Bool
isAllZeroes max seq = Prelude.all (== 0) (Seq.take max seq)

-- | Find the index and value of the leftmost (first) non-zero element in a sequence
-- Returns Nothing if all elements are zero
-- Uses pattern matching with :<| to destructure the sequence (head and tail)
indexAndValueNonZero :: Seq Rational -> Maybe IndexAndValueLeftMostNonZero
indexAndValueNonZero Seq.Empty = Nothing
indexAndValueNonZero (h :<| seq)
  | h == 0 =
     -- If head is zero, recursively check tail and increment the index
     case indexAndValueNonZero seq of
       Just (BiggestLeftMostTuple index value) -> Just (BiggestLeftMostTuple (index + 1) value)
       _ -> Nothing
  | otherwise = Just (BiggestLeftMostTuple 0 h)  -- Found non-zero at position 0

-- | Sort matrix rows starting from a given row index
-- Rows above startingRowIndex remain in place, rows from that index onward are sorted
-- Sorting is by the position of leftmost non-zero element (pivot selection)
-- Rows with leftmost non-zero elements in earlier columns come first
sortByBiggestLeftmost :: Int -> Seq (Seq Rational) -> Seq (Seq Rational)
sortByBiggestLeftmost startingRowIndex matrix =
  let (left, right) = Seq.splitAt startingRowIndex matrix
   in left >< Seq.sortBy (compare `on` indexAndValueNonZero) right

-- | Row operation: multiply first row by factor and add to second row
-- Used in Gauss-Jordan elimination to zero out elements below/above the pivot
-- Formula: row2 = factor * row1 + row2
multiplyAndAddRows :: Rational -> Seq Rational -> Seq Rational -> Seq Rational
multiplyAndAddRows factor = Seq.zipWith (\v1 v2 -> (factor * v1) + v2)

-- | Get the multiplicative inverse of the leftmost non-zero element in a row
-- Returns Nothing if the row is all zeros (no pivot exists)
-- Used to scale the pivot element to 1
getInverseValue :: Seq Rational -> Maybe Rational
getInverseValue row = case indexAndValueNonZero row of
                         Just (BiggestLeftMostTuple _ value) -> Just (if value == 1.0 then 1.0 else 1.0 / value)
                         Nothing -> Nothing

-- | Scale a row so that its leftmost non-zero element becomes 1 (create a pivot)
-- Only considers the first originalMatrixSide columns (left side of augmented matrix)
-- Returns Nothing if the row has no non-zero elements (singular matrix)
-- Uses splitAt to separate matrix into: rows before rowIndex, the target row, rows after
convertFirstColNonZeroToOne :: Seq (Seq Rational) -> Int -> Int -> Maybe (Seq (Seq Rational))
convertFirstColNonZeroToOne m originalMatrixSide rowIndex =
  let (h, row :<| t) = Seq.splitAt rowIndex m  -- h: rows before, row: current row, t: rows after

   in case getInverseValue (Seq.take originalMatrixSide row) of
              Just inverseValue ->  Just ((h |> Seq.mapWithIndex (\_ v -> v * inverseValue) row) >< t)
              Nothing -> Nothing

-- | Zero out all elements in the pivot column except the pivot itself
-- For each row, subtract a multiple of the pivot row to make that column element 0
-- The pivot row itself is left unchanged
-- If element is already 0, skip that row (optimization)
convertCellsInPivotColToZero :: Int -> Int -> Seq (Seq Rational) -> Maybe (Seq (Seq Rational))
convertCellsInPivotColToZero rowIndex originalMatrixSide m =
  let pivotRow  = m `Seq.index` rowIndex
   in case indexAndValueNonZero (Seq.take originalMatrixSide pivotRow) of
        Just (BiggestLeftMostTuple pivotCol _) -> 
          Just (Seq.mapWithIndex 
                 (\idx row -> 
                   if (idx == rowIndex) || (row `Seq.index` pivotCol) == 0.0 
                   then row  -- Don't modify pivot row or rows already zeroed
                   else multiplyAndAddRows (-1.0 * (row `Seq.index` pivotCol)) pivotRow row)  -- Zero out this element
                 m)
        Nothing -> Nothing

-- | Execute one step of Gauss-Jordan elimination for a given row/column index
-- Each step: 1) Sort rows to get best pivot, 2) Scale pivot to 1, 3) Zero out column
-- Returns Nothing if matrix is singular (no valid pivot found)
gaussJordanStep :: Int -> Maybe (Seq (Seq Rational)) -> Int -> Maybe (Seq (Seq Rational))
gaussJordanStep _ Nothing _ = Nothing
gaussJordanStep originalMatrixSide (Just matrix) index =  case convertFirstColNonZeroToOne (sortByBiggestLeftmost index matrix) originalMatrixSide index of
                                                      Just x ->
                                                                 convertCellsInPivotColToZero
                                                                           index
                                                                           originalMatrixSide
                                                                           x
                                                      Nothing -> Nothing

-- | Compute the inverse of a matrix using the Gauss-Jordan elimination algorithm
-- Algorithm:
--   1. Create augmented matrix [A|I] by concatenating A with identity matrix
--   2. Apply Gauss-Jordan elimination to transform left side to identity matrix
--   3. The right side becomes the inverse: [I|A⁻¹]
--   4. Extract the right side as the result
-- Returns Nothing if the matrix is singular (non-invertible)
inverseGaussJordan :: Matrix -> Maybe Matrix
inverseGaussJordan theMatrix@(Matrix seq) =
  let partitionedMatrix = concatenate theMatrix (buildIdentityMatrix theMatrix)  -- Create [A|I]
      originalMatrixSide = Seq.length seq
      -- Apply Gauss-Jordan elimination for each row (0 to n-1)
      invertedPartitionMatrixMaybe =
        Data.List.foldl
          (gaussJordanStep originalMatrixSide)
          (Just partitionedMatrix)
          [0 .. (originalMatrixSide -1)]
  in case invertedPartitionMatrixMaybe of
             -- Extract right half (the inverse) by dropping first originalMatrixSide columns
             Just invertedPartitionMatrix -> Just $ Matrix (Seq.mapWithIndex (\_ row -> Seq.drop originalMatrixSide row) invertedPartitionMatrix)
             Nothing -> Nothing  -- Matrix is singular (no inverse exists)
