{-# LANGUAGE LambdaCase #-}

module FrogInMaze(
  Position(..),
  Cell(..),
  Maze(..),
  newCell,
  addTunnel,
  addRow,
  buildMaze,
  buildProbabilitiesGraph,
  fromStartToExit
) where

import Data.Sequence as Seq
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Set as Set
import Data.Function as Function
data Position = Position Int Int deriving(Eq,Show,Ord)
data Cell = Free  | Mine  | Obstacle  | Exit  | Initial  | Tunnel Position  deriving(Eq,Show)
data MazeBuilder = MazeBuilder (Maybe Position) (Seq (Seq Cell))
data Maze = EmptyMaze | Maze Position (Seq (Seq Cell))  deriving(Eq,Show)
data Node = EmptyNode | Node Cell Position Double [Position] deriving(Eq,Show)
data ProbabilityGraph = EmptyGraph | ProbabilityGraph Position (Seq (Seq Node)) deriving(Show)
newtype Matrix = Matrix (Seq (Seq Double)) deriving Show


-- HsFunTy
genericAdd op (Matrix seq1) (Matrix seq2) =  Matrix $ Seq.zipWith (Seq.zipWith op) seq1 seq2
-- HsFunTy
add = genericAdd (+)
-- HsFunTy
subtractMatrix = genericAdd (-)
-- HsFunTy
getColumn i (Matrix seq) = Seq.mapWithIndex (\ _ v -> v `Seq.index` i) seq
-- HsFunTy
getRow i (Matrix seq) = seq `Seq.index` i
-- HsFunTy
multiply (Matrix seq1) m2@(Matrix _) =
  let
    getCellMultiplication row column = Data.Foldable.sum $ Seq.zipWith (*) row column
    getRowMultiplication currentRow = Seq.fromList $ Data.List.map (\ i -> getCellMultiplication currentRow (getColumn i m2)) [0..Seq.length currentRow -1]
  in
   Matrix $ Seq.mapWithIndex (\ _ currentRow -> getRowMultiplication currentRow) seq1
-- HsFunTy
divideBy (Matrix seq) n = Matrix $ Seq.mapWithIndex (\ _ row -> Seq.mapWithIndex (\ _ entry -> entry / n) row) seq
-- HsFunTy
det m@(Matrix seq)
 | Seq.length seq == 1 =  let
                            row = (seq `Seq.index` 0)
                          in (row `Seq.index` 0)
 | otherwise = Data.List.sum $ Data.List.map (\ i -> let
                                                       v1 :: Double
                                                       v1 =  (-1.0) ^ (i + 1)
                                                       value =  ((seq `Seq.index` i) `Seq.index` 0)
                                                       nextDet = det (minor m i 0)
                                                     in v1 * value * nextDet
                                             )  [0..Seq.length seq -1]
-- HsFunTy
minor (Matrix seq) i j =
  let
   filterJ:: Seq a -> Seq a
   filterJ row = Seq.foldlWithIndex (\ acc index value -> if index /= j then  acc |> value else acc) Seq.empty row
  in Matrix $ Seq.foldlWithIndex (\ acc currentRowIndex row -> if i /= currentRowIndex then acc |> filterJ row else acc) Seq.empty seq
-- HsFunTy
cofactor m@(Matrix seq) = Matrix $ Seq.mapWithIndex (\ i row ->  Seq.mapWithIndex (\ j _ -> ((-1) ^ (i + j + 1)) * det (minor m i j)) row) seq
-- HsFunTy
transpose_ m@(Matrix seq) = Matrix (Seq.fromList (Data.List.map (`getColumn` m) [0..Seq.length seq -1]))
-- HsFunTy
adjugate = transpose_ . cofactor
-- HsFunTy
inverse m@(Matrix seq) =
 case det m of
   0.0 -> Nothing
   d -> Just $ adjugate m `divideBy` d
-- HsFunTy
isAbsorbingState :: Node -> Int
isAbsorbingState (Node Exit _ _ _) = 1
isAbsorbingState (Node Mine _ _ _) = 1
isAbsorbingState (Node Obstacle _ _ _) = 0
isAbsorbingState (Node _ _ 0.0 []) = 1
isAbsorbingState _ = 0
-- HsFunTy
partitionNodes :: ProbabilityGraph -> (Int, Seq Node)
partitionNodes (ProbabilityGraph _ seq) =
  let
      sortedNodes :: Seq (Int,Node)
      sortedNodes = Seq.sortBy
          (compare `Function.on` negate . (\ (_,node)  -> isAbsorbingState node))
          (Seq.foldlWithIndex (\ acc _ row -> Seq.foldlWithIndex (\acc1 _ value -> acc1 |> (isAbsorbingState value,value)) acc row) Seq.empty seq)
  in Seq.foldlWithIndex (\ acc _ current -> let
                                              (i,currentNode) = current
                                              (total,nodes) = acc
                                            in (i + total, nodes |> currentNode)) (0,Seq.empty) sortedNodes
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
-- HsFunTy                    
getNode n (ProbabilityGraph _ nodes ) = let colCount = Seq.length (nodes `Seq.index` 0)
                                            i = (n `div` colCount)
                                            j = (n `mod` colCount)
                                        in ((nodes `Seq.index` i) `Seq.index` j)
buildIdentityMetrix (Matrix seq) = Matrix $ Seq.mapWithIndex (\ i row ->  Seq.mapWithIndex (\ j value -> if i == j then 1 else 0) row) seq                                        
-- HsAppTy
fromStartToExit p = let     
                                     (countAbsorvingStates, sortedNodes) = partitionNodes p
                                     matrixSide = [0..Seq.length sortedNodes -1]
                                     standardFormMatrix = Seq.fromList $ Data.List.map (\ i -> Seq.fromList (Data.List.map (\ j -> getProbabilityOfTransition (sortedNodes `Seq.index` i) (sortedNodes `Seq.index` j)) matrixSide))  matrixSide
                                     idMatrix = Matrix $ Seq.mapWithIndex (\ _ row -> Seq.take countAbsorvingStates row) (Seq.take countAbsorvingStates standardFormMatrix)
                                     nonAbsorvingRows = Seq.drop countAbsorvingStates standardFormMatrix
                                     startingNode = getStartingPositionNode p
                                     startingNodeIndex = indexOf startingNode sortedNodes
                                     r =  Matrix $ Seq.mapWithIndex (\ _ row -> Seq.take countAbsorvingStates row) nonAbsorvingRows
                                     q =  Matrix $ Seq.mapWithIndex (\ _ row -> Seq.drop countAbsorvingStates row) nonAbsorvingRows
                                     fundamentalMatrix = inverse (buildIdentityMetrix q `subtractMatrix` q)
                                     fr = case fundamentalMatrix of
                                            Just m -> Just $ m `multiply` r
                                            Nothing -> Nothing
                             in  case fr of 
                                    Just matrix ->  Data.Foldable.sum $ 
                                                      Seq.mapWithIndex (\ _ (_,p) -> p) $ 
                                                      Seq.filter (\case {(Node Exit _ _ _,_) -> True; _ -> False})  $ 
                                                      Seq.zip (Seq.take countAbsorvingStates sortedNodes) 
                                                              (Seq.take countAbsorvingStates (getRow (startingNodeIndex - countAbsorvingStates) matrix)) 
                                    Nothing -> 0.0


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
buildProbabilitiesGraph maze@(Maze start cells) =
  let 
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
