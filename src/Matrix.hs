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

getCell i j m = getRow i m `Seq.index` j 


getDimensions m@(Matrix seq) = let rowsCount = Seq.length seq
                                   colsCount =  if rowsCount > 0 then Seq.length (getRow 0 m) else 0 
                                in (rowsCount,colsCount)

-- HsFunTy
multiply (Matrix seq1) m2@(Matrix seq2) =
  let getCellMultiplication row column = Data.Foldable.sum $ Seq.zipWith (*) row column
      getRowMultiplication currentRow = Seq.fromList $ Data.List.map (\i -> getCellMultiplication currentRow (getColumn i m2)) [0 .. Seq.length (seq2 `Seq.index` 0) -1]
   in Matrix $ Seq.mapWithIndex (\_ currentRow -> getRowMultiplication currentRow) seq1

-- HsFunTy
divideBy (Matrix seq) n = Matrix $ Seq.mapWithIndex (\_ row -> Seq.mapWithIndex (\_ entry -> entry / n) row) seq

subMatrix startX startY endX endY (Matrix seq) =  Matrix $ Seq.mapWithIndex  (\ _ row -> Seq.take (endY - startY) (Seq.drop startY row))  
                                                                             (Seq.take (endX - startX) (Seq.drop startX seq))

-- HsFunTy
transpose_ m@(Matrix seq) = Matrix (Seq.fromList (Data.List.map (`getColumn` m) [0 .. Seq.length seq -1]))


buildIdentityMatrix (Matrix seq) = Matrix $ Seq.mapWithIndex (\i row -> Seq.mapWithIndex (\j _ -> if i == j then 1 else 0) row) seq

isIdentityMatrix (Matrix seq) = 
  let
    isIdentityRow j row = Seq.foldlWithIndex (\ acc index value -> acc && (value == (if index == j then 1 else 0))) True row
  in Seq.foldlWithIndex (\ acc index row -> acc && isIdentityRow index row) True seq


concatenate (Matrix seq1) (Matrix seq2) = Seq.zipWith (><) seq1 seq2

isAllZeroes max seq = Prelude.all (== 0) (Seq.take max seq)

indexAndValueNonZero Seq.Empty = Nothing
indexAndValueNonZero (h :<| seq)
  | h == 0 =
     case indexAndValueNonZero seq of 
       Just (BiggestLeftMostTuple index value) -> Just (BiggestLeftMostTuple (index + 1) value)
       _ -> Nothing 
  | otherwise = Just (BiggestLeftMostTuple 0 h)

sortByBiggestLeftmost startingRowIndex matrix =
  let (left, right) = Seq.splitAt startingRowIndex matrix
   in left >< Seq.sortBy (compare `on` indexAndValueNonZero) right

multiplyAndAddRows factor = Seq.zipWith (\v1 v2 -> (factor * v1) + v2)

getInverseValue row = case indexAndValueNonZero row of
                         Just (BiggestLeftMostTuple _ value) -> Just (if value == 1.0 then 1.0 else 1.0 / value)
                         Nothing -> Nothing

convertFirstColNonZeroToOne m originalMatrixSide rowIndex =
  let (h, row :<| t) = Seq.splitAt rowIndex m
      
   in case getInverseValue (Seq.take originalMatrixSide row) of
              Just inverseValue ->  Just ((h |> Seq.mapWithIndex (\_ v -> v * inverseValue) row) >< t)
              Nothing -> Nothing
   
convertCellsInPivotColToZero rowIndex originalMatrixSide m =
  let pivotRow  = m `Seq.index` rowIndex
   in case indexAndValueNonZero (Seq.take originalMatrixSide pivotRow) of
        Just (BiggestLeftMostTuple pivotCol _) -> Just (Seq.mapWithIndex (\idx row -> if (idx == rowIndex) || (row `Seq.index` pivotCol) == 0.0 then row else multiplyAndAddRows (-1.0 * (row `Seq.index` pivotCol)) pivotRow row) m)
        Nothing -> Nothing
        
gaussJordanStep _ Nothing _ = Nothing
gaussJordanStep originalMatrixSide (Just matrix) index =  case (convertFirstColNonZeroToOne (sortByBiggestLeftmost index matrix) originalMatrixSide index) of
                                                      Just x ->    
                                                                 convertCellsInPivotColToZero
                                                                           index
                                                                           originalMatrixSide
                                                                           x
                                                      Nothing -> Nothing
                                                             
  

inverseGaussJordan theMatrix@(Matrix seq) =
  let partitionedMatrix = concatenate theMatrix (buildIdentityMatrix theMatrix)
      originalMatrixSide = Seq.length seq
      invertedPartitionMatrixMaybe =
        Data.List.foldl
          (gaussJordanStep originalMatrixSide)
          (Just partitionedMatrix)
          [0 .. (originalMatrixSide -1)]
  in case invertedPartitionMatrixMaybe of
             Just invertedPartitionMatrix -> Just $ Matrix (Seq.mapWithIndex (\_ row -> Seq.drop originalMatrixSide row) invertedPartitionMatrix)
             Nothing -> Nothing
