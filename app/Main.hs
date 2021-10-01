{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances, LambdaCase #-}

module Main where
import GHC.Stack
import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set as Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Sequence as Seq
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Set as Set
import Data.List
import Data.Function as Function
import Numeric
import Text.Printf


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


data Position = Position Int Int deriving(Eq,Show,Ord)
data Cell = Free  | Mine  | Obstacle  | Exit  | Initial  | Tunnel Position  deriving(Eq,Show)
data MazeBuilder = MazeBuilder (Maybe Position) (Seq (Seq Cell))
data Maze = EmptyMaze | Maze Position (Seq (Seq Cell))  deriving(Eq,Show)
data Node = EmptyNode | Node Cell Position Rational [Position] deriving(Eq,Show)
data ProbabilityGraph = EmptyGraph | ProbabilityGraph Position (Seq (Seq Node)) deriving(Show)


-- HsFunTy
isAbsorbingState :: Node -> Int
isAbsorbingState (Node Exit _ _ _) = 1
isAbsorbingState (Node Mine _ _ _) = 1
isAbsorbingState (Node _ _ 0.0 []) = 1
isAbsorbingState _ = 0
-- HsFunTy
isEmptyNode EmptyNode = True
isEmptyNode _ = False
partitionNodes :: ProbabilityGraph -> (Int, Seq Node)
partitionNodes (ProbabilityGraph _ seq) =
  let
      sortedNodes :: Seq (Int,Node)
      sortedNodes = Seq.filter (\ (_,x) -> not (isEmptyNode x))  $ Seq.sortBy
          (compare `Function.on` negate . fst)
          (Seq.foldlWithIndex (\ acc _ row -> Seq.foldlWithIndex (\acc1 _ value -> acc1 |> (isAbsorbingState value,value)) acc row) Seq.empty seq)
  in Seq.foldlWithIndex (\ acc _ current -> let
                                              (i,currentNode) = current
                                              (total,nodes) = acc
                                            in (i + total, nodes |> currentNode)) (0,Seq.empty) sortedNodes

hasExits (Maze _ seq) =   Data.Maybe.isJust $ Seq.findIndexL (\case Exit-> True; _ -> False) $ seq >>= id
                                                        
-- HsFunTy
getProbabilityOfTransition _ EmptyNode = 0.0
getProbabilityOfTransition EmptyNode _ = 0.0
getProbabilityOfTransition (Node _ pos1 prob neighbors) n2@(Node _ pos2 _ _)
  | pos2 `Data.List.elem` neighbors = prob
  | pos1 == pos2 = fromIntegral $ isAbsorbingState n2
  | otherwise = 0.0

-- HsFunTy
getStartingPositionNode (ProbabilityGraph (Position i j) nodes) = (nodes `Seq.index` i) `Seq.index` j
indexOf node nodes = let newSeq = Seq.takeWhileL (/= node)  nodes
                     in if Seq.length newSeq == Seq.length nodes then -1 else Seq.length newSeq

getFundamentalMatrix m = inverseGaussJordan ((buildIdentityMatrix m) `subtractMatrix` m)
-- HsAppTy
fromStartToExit:: ProbabilityGraph -> Double
fromStartToExit EmptyGraph = 0
fromStartToExit p = let     
                                     (countAbsorvingStates, sortedNodes) = partitionNodes p
                                     matrixSide = [0..Seq.length sortedNodes -1]
                                     standardFormMatrix = Seq.fromList $ Data.List.map (\ i -> Seq.fromList (Data.List.map (\ j -> getProbabilityOfTransition (sortedNodes `Seq.index` i) (sortedNodes `Seq.index` j)) matrixSide))  matrixSide
                                     nonAbsorvingRows = Seq.drop countAbsorvingStates standardFormMatrix
                                     startingNode = getStartingPositionNode p
                                     startingNodeIndex = indexOf startingNode sortedNodes
                                     r =  Matrix $ Seq.mapWithIndex (\ _ row -> Seq.take countAbsorvingStates row) nonAbsorvingRows
                                     q =  Matrix $ Seq.mapWithIndex (\ _ row -> Seq.drop countAbsorvingStates row) nonAbsorvingRows
                                     fundamentalMatrix = getFundamentalMatrix q
                                     fr = case fundamentalMatrix of
                                            Just m -> Just $ m `multiply` r
                                            Nothing -> Nothing
                             in  case fr of
                                    Just matrix ->    fromRational $
                                                      Data.Foldable.sum $
                                                      Seq.mapWithIndex (\ _ (_,p) -> p) $
                                                      Seq.filter (\case {(Node Exit _ _ _,_) -> True; _ -> False})  $
                                                      Seq.zip (Seq.take countAbsorvingStates sortedNodes)
                                                              (Seq.take countAbsorvingStates (getRow (startingNodeIndex - countAbsorvingStates) matrix))
                                    Nothing -> 0


-- HsFunTy
getNeighbors (Node _ _ _ n) = n
-- HsFunTy

addTunnel (Maze initial cells) i1 j1 i2 j2 =
  let x1 = (i1 -1)
      y1 = (j1 - 1)
      x2 = (i2 - 1)
      y2 = (j2 - 1)
      mazeUpdate :: Int -> Int -> Int -> Int -> Seq (Seq Cell) -> Seq (Seq Cell)
      mazeUpdate x_1 y_1 x_2 y_2 myCells = Seq.update x_1  (Seq.update y_1 (Tunnel (Position x_2 y_2)) (Seq.index myCells x_1)) myCells
  in  Maze initial (mazeUpdate x2 y2 x1 y1 (mazeUpdate x1 y1 x2 y2 cells))
-- HsFunTy
addRow :: MazeBuilder -> [Cell] -> MazeBuilder
addRow (MazeBuilder startingPos@(Just _) oldCells) newCells = MazeBuilder startingPos (oldCells |> Seq.fromList newCells)
addRow (MazeBuilder Nothing oldCells) newCells = case Data.List.elemIndex Initial newCells of
                                                    Nothing -> MazeBuilder Nothing (oldCells |> Seq.fromList newCells)
                                                    Just idx -> MazeBuilder (Just (Position (Seq.length oldCells) idx)) (oldCells |> Seq.fromList newCells)
-- HsFunTy
toMaze (MazeBuilder (Just position) cells) = Maze position cells
-- HsFunTy
buildMaze :: [[Cell]] -> Maze
buildMaze =  toMaze . Data.List.foldl addRow (MazeBuilder Nothing Seq.empty)
-- HsFunTy
cellAt :: Int -> Int -> Seq (Seq a) -> Maybe a
cellAt i j cells
 | i < 0 = Nothing
 | j < 0 = Nothing
 | i >= Seq.length cells = Nothing
 | j >= Seq.length (cells `Seq.index` 0) = Nothing
 | otherwise = Just ((cells `Seq.index` i) `Seq.index` j)
 
-- HsFunTy
canMove :: Int -> Int -> Maze -> Maybe (Int, Int, Maybe Cell)
canMove i j (Maze _ cells) = case cellAt i j cells of
                     Nothing -> Nothing
                     Just Obstacle -> Nothing
                     x -> Just (i,j,x)
-- HsFunTy
buildProbabilitiesGraph EmptyMaze = EmptyGraph
buildProbabilitiesGraph maze@(Maze start cells)
 | not (hasExits maze) = EmptyGraph
 | True = let 
           neighborsIndexes i j = [(i,j-1),(i,j+1),(i-1,j),(i+1,j)]
           neighbors i j  = Data.Foldable.concatMap (\ (ni,nj) -> Data.Maybe.maybeToList (canMove ni nj maze)) (neighborsIndexes i j)
           buildProbabilitiesNode :: Int -> Int -> Cell -> Node
           buildProbabilitiesNode i j cell  =  case cell of
                                              Mine -> Node Mine (Position i j) 0.0 []
                                              tunnel@(Tunnel (Position ni nj)) -> let ns = neighbors ni nj
                                                                                      nsLength = Data.List.length ns
                                                                                      neighborsProb = if nsLength == 0 then 0 else 1.0 / fromIntegral nsLength
                                                                                   in Node tunnel (Position i j) neighborsProb (Data.List.map (\ (x,y,_) -> Position x y) ns)
                                              Obstacle -> EmptyNode
                                              Exit -> Node Exit (Position i j) 0.0 []
                                              theCell -> let ns = neighbors i j
                                                             nsLength = Data.List.length ns
                                                             neighborsProb = if nsLength == 0 then 0 else 1.0 / fromIntegral nsLength
                                                               in Node theCell (Position i j) neighborsProb (Data.List.map (\ (x,y,_) -> Position x y) ns)
        in
         ProbabilityGraph start $ Seq.mapWithIndex (Seq.mapWithIndex . buildProbabilitiesNode) cells

-- HsFunTy
newCell :: Char -> Cell
newCell c =
  case c of
       'A' -> Initial
       'O' -> Free
       '*' -> Mine
       '%' -> Exit
       '#' -> Obstacle

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main = do 
    firstMultipleInputTemp <- getLine
    
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    let k = read (firstMultipleInput !! 2) :: Int

 
    let ioRows =  forM [1..n] $ \_-> do
                               line <- getLine
                               return (Data.List.map newCell $ rstrip line)
               
   
    cells <- ioRows
    let maze = buildMaze cells           
   
    tunnels <- forM [1..k] $ \_ -> do
                    secondMultipleInputTemp <- getLine
                    let secondMultipleInput = Data.List.words $ rstrip secondMultipleInputTemp

                    let i1 = read (secondMultipleInput !! 0) :: Int

                    let j1 = read (secondMultipleInput !! 1) :: Int

                    let i2 = read (secondMultipleInput !! 2) :: Int

                    let j2 = read (secondMultipleInput !! 3) :: Int

                    return (i1,j1,i2,j2)
    let toPrint = fromStartToExit (buildProbabilitiesGraph $ Data.List.foldl (\ m (i1,j1,i2,j2) -> addTunnel m i1 j1 i2 j2) maze tunnels)                
    printf "%2f\n" toPrint
 
