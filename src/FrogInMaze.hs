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

import Data.List
import Matrix
import Data.Sequence as Seq
import Data.Maybe
import Data.Foldable
import Data.Function as Function
data Position = Position Int Int deriving(Eq,Show,Ord)
data Cell = Free  | Mine  | Obstacle  | Exit  | Initial  | Tunnel Position  deriving(Eq,Show)
data MazeBuilder = MazeBuilder (Maybe Position) (Seq (Seq Cell))
data Maze = EmptyMaze | Maze Position (Seq (Seq Cell))  deriving(Eq,Show)
data Node = EmptyNode | Node Cell Position Double [Position] deriving(Eq,Show)
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

getFundamentalMatrix m = Matrix.inverseGaussJordan (buildIdentityMatrix m `subtractMatrix` m)
-- HsAppTy
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
                                    Just matrix ->    Data.Foldable.sum $
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