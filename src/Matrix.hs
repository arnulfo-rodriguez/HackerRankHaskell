module Matrix
  ( Matrix (..),
    inverse,
    subtractMatrix,
    multiply,
    getRow,
    buildIdentityMatrix,
    inverseGaussJordan,
    subMatrix
  )
where

import Data.Foldable
import Data.Function as Function
import Data.List
import Data.Sequence as Seq

newtype Matrix = Matrix (Seq (Seq Rational)) deriving (Show)

data IndexAndValueLeftMostNonZero = BiggestLeftMostTuple Int Rational deriving (Eq,Show)


instance Ord IndexAndValueLeftMostNonZero where
  (BiggestLeftMostTuple index1 value1) `compare` (BiggestLeftMostTuple index2 value2) = case index1 `compare` index2 of
    EQ -> value2 `compare` value1
    comp -> comp

-- HsFunTy
genericAdd op (Matrix seq1) (Matrix seq2) = Matrix $ Seq.zipWith (Seq.zipWith op) seq1 seq2

-- HsFunTy
add = genericAdd (+)

-- HsFunTy
subtractMatrix = genericAdd (-)

-- HsFunTy
getColumn i (Matrix seq) = Seq.mapWithIndex (\_ v -> v `Seq.index` i) seq

-- HsFunTy
getRow i (Matrix seq) = seq `Seq.index` i

-- HsFunTy
multiply (Matrix seq1) m2@(Matrix seq2) =
  let getCellMultiplication row column = Data.Foldable.sum $ Seq.zipWith (*) row column
      getRowMultiplication currentRow = Seq.fromList $ Data.List.map (\i -> getCellMultiplication currentRow (getColumn i m2)) [0 .. Seq.length (seq2 `index` 0) -1]
   in Matrix $ Seq.mapWithIndex (\_ currentRow -> getRowMultiplication currentRow) seq1

-- HsFunTy
divideBy (Matrix seq) n = Matrix $ Seq.mapWithIndex (\_ row -> Seq.mapWithIndex (\_ entry -> entry / n) row) seq

-- HsFunTy
det m@(Matrix seq)
  | Seq.length seq == 1 =
    let row = (seq `Seq.index` 0)
     in (row `Seq.index` 0)
  | otherwise =
    Data.List.sum $
      Data.List.map
        ( \i ->
            let v1 :: Rational
                v1 = (-1.0) ^ (i + 1)
                value = ((seq `Seq.index` i) `Seq.index` 0)
                nextDet = det (minor m i 0)
             in v1 * value * nextDet
        )
        [0 .. Seq.length seq -1]

-- HsFunTy
minor (Matrix seq) i j =
  let filterJ :: Seq a -> Seq a
      filterJ row = Seq.foldlWithIndex (\acc index value -> if index /= j then acc |> value else acc) Seq.empty row
   in Matrix $ Seq.foldlWithIndex (\acc currentRowIndex row -> if i /= currentRowIndex then acc |> filterJ row else acc) Seq.empty seq

subMatrix startX startY endX endY (Matrix seq) =  Matrix $ Seq.mapWithIndex  (\ _ row -> Seq.take (endY - startY) (Seq.drop startY row))  
                                                                             (Seq.take (endX - startX) (Seq.drop startX seq))

-- HsFunTy
cofactor m@(Matrix seq) = Matrix $ Seq.mapWithIndex (\i row -> Seq.mapWithIndex (\j _ -> ((-1) ^ (i + j + 1)) * det (minor m i j)) row) seq

-- HsFunTy
transpose_ m@(Matrix seq) = Matrix (Seq.fromList (Data.List.map (`getColumn` m) [0 .. Seq.length seq -1]))

-- HsFunTy
adjugate = transpose_ . cofactor

buildIdentityMatrix (Matrix seq) = Matrix $ Seq.mapWithIndex (\i row -> Seq.mapWithIndex (\j _ -> if i == j then 1 else 0) row) seq

isIdentityMatrix (Matrix seq) = 
  let
    isIdentityRow j row = Seq.foldlWithIndex (\ acc index value -> acc && (value == (if index == j then 1 else 0))) True row
  in Seq.foldlWithIndex (\ acc index row -> acc && isIdentityRow index row) True seq

-- HsFunTy
inverse m@(Matrix seq) =
  case det m of
    0.0 -> Nothing
    d -> Just $ adjugate m `divideBy` d

concatenate (Matrix seq1) (Matrix seq2) = Seq.zipWith (><) seq1 seq2

isAllZeroes max seq = Prelude.all (== 0) (Seq.take max seq)

indexAndValueNonZero Seq.Empty = error "all zeros row"
indexAndValueNonZero (h :<| seq)
  | h == 0 =
    let (BiggestLeftMostTuple index value) = indexAndValueNonZero seq
     in BiggestLeftMostTuple (index + 1) value
  | otherwise = BiggestLeftMostTuple 0 h

sortByBiggestLeftmost startingRowIndex matrix =
  let (left, right) = Seq.splitAt startingRowIndex matrix
   in left >< Seq.sortBy (compare `on` indexAndValueNonZero) right

multiplyAndAddRows factor = Seq.zipWith (\v1 v2 -> (factor * v1) + v2)


convertFirstColNonZeroToOne m originalMatrixSide rowIndex =
  let (h, row :<| t) = Seq.splitAt rowIndex m
      (BiggestLeftMostTuple _ value) = indexAndValueNonZero (Seq.take originalMatrixSide row)
      inverseValue =  if value == 1.0 then 1.0 else 1.0 / value
   in (h |> Seq.mapWithIndex (\_ v -> v * inverseValue) row) >< t
   
convertCellsInPivotColToZero rowIndex originalMatrixSide m =
  let pivotRow  = m `Seq.index` rowIndex
      (BiggestLeftMostTuple pivotCol _) = indexAndValueNonZero (Seq.take originalMatrixSide pivotRow)
      transformedRows = Seq.mapWithIndex (\idx row -> if (idx == rowIndex) || (row `Seq.index` pivotCol) == 0.0 then row else multiplyAndAddRows (-1.0 * (row `Seq.index` pivotCol)) pivotRow row) m
   in transformedRows

gaussJordanStep originalMatrixSide matrix index =  convertCellsInPivotColToZero
                                                             index
                                                             originalMatrixSide
                                                             (convertFirstColNonZeroToOne (sortByBiggestLeftmost index matrix) originalMatrixSide index)
  

inverseGaussJordan theMatrix@(Matrix seq) =
  let partitionedMatrix = concatenate theMatrix (buildIdentityMatrix theMatrix)
      originalMatrixSide = Seq.length seq
      invertedPartitionMatrix =
        Data.List.foldl
          (gaussJordanStep originalMatrixSide)
          partitionedMatrix
          [0 .. (originalMatrixSide -1)]
      inverted =  Seq.mapWithIndex (\_ row -> Seq.drop originalMatrixSide row) invertedPartitionMatrix
   in Just $ Matrix inverted
