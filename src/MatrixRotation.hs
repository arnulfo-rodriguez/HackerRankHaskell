{-# LANGUAGE TupleSections #-}

module MatrixRotation where

import Data.Sequence as Seq
import Data.List as List
import Matrix
import Data.Ratio as Ratio

data LayeredMatrix = EmptyLayeredMatrix | LayeredMatrix Int Int [Int]  LayeredMatrix

data TraverseStatus = TopRow | LeftCol | BottomRow | RightCol


outerLayerIndexes n m =
  let
    topRow = List.map (0,) [0..(m-1)]
    leftCol =   List.map (, 0) [(n-2),(n - 3)..1] 
    bottomRow = List.map (n - 1,) [(m-1),(m - 2)..0]
    rightCol = List.map (, m - 1) [1..(n-2)]
  in Seq.fromList $ topRow ++ rightCol ++ bottomRow ++ leftCol

mapIndexesToRealPosition layer = Seq.mapWithIndex (\ _ (x,y) -> (x + layer, y + layer))

shiftLeft turns s =
  let realTurns = turns `mod` Seq.length s
      (first,second) = Seq.splitAt realTurns s
  in second >< first

toRowsAndCols seq n m =
  let
    (topRow,rem1) =  Seq.splitAt m seq
    (rightCol,rem2) = Seq.splitAt (n - 2) rem1
    (bottomRow,leftCol) = Seq.splitAt m rem2
  in (topRow,Seq.reverse leftCol,Seq.reverse bottomRow,rightCol)

getMatrixRow :: Seq (Seq Rational,Seq Rational,Seq Rational,Seq Rational) -> Int -> Seq Rational
getMatrixRow Empty _ = Empty
getMatrixRow  ((topRow,_,_,_) :<| _) 0 = topRow
getMatrixRow  ((_,leftCol,bottomRow,rightCol) :<| rest) n
 | n <= Seq.length leftCol = ((leftCol `Seq.index` (n - 1)) <| getMatrixRow rest (n - 1)) |> (rightCol `Seq.index` (n - 1))
 | otherwise = bottomRow

toCellValues:: Matrix ->  Seq (Int,Int) -> Seq Rational
toCellValues m = Seq.mapWithIndex (\ _ (x, y) -> getCell x y m)

matrixRotation :: Matrix -> Int -> Matrix
matrixRotation m r =
  let
    (rowCount,colCount) = getDimensions m
    numberOfNestedMatrices = rowCount `div` 2
    listsOfIndexes :: [Seq(Int,Int)]
    listsOfIndexes = List.map (\ i ->  mapIndexesToRealPosition i (outerLayerIndexes (rowCount - (2*i)) (colCount - (2*i))))  [0 .. (numberOfNestedMatrices - 1)]
    innerLists =  List.map (toCellValues m) listsOfIndexes
    rotatedLists = Seq.fromList $ List.map (shiftLeft r) innerLists
    rowsAndCols = Seq.mapWithIndex (\ i s -> toRowsAndCols s (rowCount - (2*i)) (colCount - (2*i))) rotatedLists
    newRows = Seq.fromList $ List.map (getMatrixRow rowsAndCols)  [0..rowCount -1]
  in Matrix newRows