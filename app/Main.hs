{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances, LambdaCase, TupleSections #-}

module Main where
import GHC.Stack
import Control.Monad
import Data.Array
import Data.Bits
import Data.Set as Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Sequence as Seq
import Data.Maybe
import Data.Foldable
import Data.Function as Function
import Numeric
import Data.Text.Read
import Data.List as List
import Data.Ratio as Ratio
import Text.Printf as Printf
import Control.Monad as Monad

newtype Matrix = Matrix (Seq (Seq Rational)) deriving (Show)
getRow i (Matrix seq) = seq `Seq.index` i

getCell i j m = getRow i m `Seq.index` j


getDimensions m@(Matrix seq) = let rowsCount = Seq.length seq
                                   colsCount =  if rowsCount > 0 then Seq.length (getRow 0 m) else 0
                                in (rowsCount,colsCount)

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

showMatrix (Matrix sq) =
  let
      showRow :: Seq Rational -> IO (Seq ())
      showRow row = forM row (\ number -> do
                                            putStr $ printf "%d " (Ratio.numerator number))
  in forM sq (\ row ->
                   do
                     _ <- showRow row
                     putStrLn ""
                     )

matrixRotation :: Matrix -> Int -> Matrix
matrixRotation m r =
  let
    (rowCount,colCount) = getDimensions m
    numberOfNestedMatrices = min (rowCount `div` 2) (colCount `div` 2)
    listsOfIndexes :: [Seq(Int,Int)]
    listsOfIndexes = List.map (\ i ->  mapIndexesToRealPosition i (outerLayerIndexes (rowCount - (2*i)) (colCount - (2*i))))  [0 .. (numberOfNestedMatrices - 1)]
    innerLists =  List.map (toCellValues m) listsOfIndexes
    rotatedLists = Seq.fromList $ List.map (shiftLeft r) innerLists
    rowsAndCols = Seq.mapWithIndex (\ i s -> toRowsAndCols s (rowCount - (2*i)) (colCount - (2*i))) rotatedLists
    newRows = Seq.fromList $ List.map (getMatrixRow rowsAndCols)  [0..rowCount -1]
  in Matrix newRows

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

toRationals::String-> Seq Rational
toRationals line = Seq.fromList $ List.map (toRational.read) $ List.words $ rstrip line

main = do 
    firstMultipleInputTemp <- getLine
    
    let firstMultipleInput = List.words $ rstrip firstMultipleInputTemp

    let m = read (firstMultipleInput !! 0) :: Int

    let n = read (firstMultipleInput !! 1) :: Int

    let r = read (firstMultipleInput !! 2) :: Int

 
    ioRows <- forM [1..m] $ \_-> do
                               line <- getLine
                               return $ toRationals line
               
    let matrix = Matrix $ Seq.fromList ioRows           
    showMatrix $ matrixRotation matrix r
 
