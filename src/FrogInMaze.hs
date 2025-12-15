{-# LANGUAGE LambdaCase #-}

{-|
Module      : FrogInMaze
Description : Solution for the Frog in Maze probability problem from HackerRank

== Problem Statement

A frog starts at position 'A' in a maze and can move randomly to adjacent cells.
The maze contains:
- 'A': Starting position (Initial)
- 'O': Open cells (Free)
- '%': Exit cells (the goal)
- '*': Mines (absorbing state - frog dies)
- '#': Obstacles (cannot move through)
- Tunnels: Bidirectional connections between non-adjacent cells

The frog moves randomly with equal probability to any accessible adjacent cell.
When the frog reaches an exit or mine, it stops (absorbing states).

Goal: Calculate the probability that the frog reaches an exit before hitting a mine
or getting stuck.

== Algorithm Overview

This solution uses Absorbing Markov Chain analysis:

1. **Build Maze Model**: Parse the grid into a graph of connected positions
   - Identify tunnels (bidirectional teleportation)
   - Track which cells are reachable from start

2. **Build Probability Graph**: Convert maze into a Markov chain
   - Each position is a state
   - Transition probability to each neighbor = 1/n where n = number of neighbors
   - Exit and Mine cells are absorbing states (probability 0 of leaving)

3. **Standard Form Matrix**: Rearrange transition matrix into canonical form
   - Top rows: Absorbing states (Exit, Mine, dead-ends)
   - Bottom rows: Transient states (can still transition)
   - Split into submatrices: Q (transient→transient), R (transient→absorbing)

4. **Fundamental Matrix**: Calculate N = (I - Q)^(-1)
   - N[i,j] = expected number of times in state j when starting from state i
   - Use Gauss-Jordan elimination to compute matrix inverse

5. **Absorption Probabilities**: Calculate B = N × R
   - B[i,j] = probability of being absorbed into state j when starting from state i
   - Sum probabilities for all Exit states

== Mathematical Background

For an absorbing Markov chain with transition matrix in standard form:
```
P = | I   R |
    | 0   Q |
```

The fundamental matrix N = (I - Q)^(-1) gives expected visits to transient states.
The absorption probability matrix B = N × R gives probabilities of ending in each
absorbing state.

== Time Complexity

O(V³) where V = number of reachable cells (due to matrix inversion)
Space: O(V²) for storing the transition matrix

-}
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
import Data.Set as Set
import Data.List
import Matrix
import Data.Maybe
import Data.Foldable
import Data.Function as Function

-- | Represents a 2D position in the maze (row, column)
data Position = Position Int Int deriving(Eq,Show,Ord)

-- | Represents the type of cell in the maze
-- Free: Open cell the frog can move through
-- Mine: Absorbing state - frog dies
-- Obstacle: Cannot pass through
-- Exit: Goal state - frog escapes successfully
-- Initial: Starting position
-- Tunnel: Teleports to another position, contains original cell type
data Cell = Free  | Mine  | Obstacle  | Exit  | Initial  | Tunnel Position Cell deriving(Eq,Show)

-- | Builder for constructing mazes incrementally
-- Tracks starting position (if found) and accumulated rows
data MazeBuilder = MazeBuilder (Maybe Position) (Seq (Seq Cell))

-- | Represents a complete maze
-- EmptyMaze: Invalid/empty maze
-- Maze: Contains starting position and grid of cells
data Maze = EmptyMaze | Maze Position (Seq (Seq Cell))  deriving(Eq,Show)

-- | Represents a node in the probability graph (Markov chain state)
-- EmptyNode: Placeholder for obstacles
-- Node: Contains cell type, position, transition probability, and neighbor positions
--   - Cell: Type of cell at this position
--   - Position: Location in maze
--   - Rational: Probability of moving to each neighbor (1/n where n = neighbor count)
--   - [Position]: List of neighboring positions
data Node = EmptyNode | Node Cell Position Rational [Position] deriving(Eq,Show)

-- | Represents the complete Markov chain for the maze
-- EmptyGraph: Invalid graph (no exits or empty maze)
-- ProbabilityGraph: Contains starting position and all reachable nodes
data ProbabilityGraph = EmptyGraph | ProbabilityGraph Position [Node] deriving(Show)

-- | Checks if a node is an absorbing state (cannot leave)
-- Returns 1 if absorbing, 0 otherwise
-- Absorbing states: Exit, Mine, or dead-ends (no neighbors)
isAbsorbingState :: Node -> Int
isAbsorbingState (Node Exit _ _ _) = 1
isAbsorbingState (Node Mine _ _ _) = 1
isAbsorbingState (Node _ _ 0.0 []) = 1
isAbsorbingState _ = 0

-- | Checks if a node is empty (obstacle)
isEmptyNode EmptyNode = True
isEmptyNode _ = False

-- | Partitions nodes into absorbing and transient states
-- Returns (count of absorbing states, sorted sequence of all non-empty nodes)
-- Nodes are sorted with absorbing states first, then transient states
-- This creates the standard form for the Markov chain transition matrix
partitionNodes :: ProbabilityGraph -> (Int, Seq Node)
partitionNodes (ProbabilityGraph _ seq) =
  let
      sortedNodes :: Seq (Int,Node)
      sortedNodes = Seq.filter (\ (_,x) -> not (isEmptyNode x))  $ Seq.sortBy
          (compare `Function.on` negate . fst)
          (Seq.fromList (Data.List.map (\ value -> (isAbsorbingState value,value)) seq))
  in Seq.foldlWithIndex (\ acc _ current -> let
                                              (i,currentNode) = current
                                              (total,nodes) = acc
                                            in (i + total, nodes |> currentNode)) (0,Seq.empty) sortedNodes

-- | Checks if the maze has at least one exit cell
-- Returns False if no exit exists (no solution possible)
hasExits (Maze _ seq) =   Data.Maybe.isJust $ Seq.findIndexL (\case Exit-> True; _ -> False) $ seq >>= id

-- | Gets the transition probability from node n1 to node n2
-- Returns 0.0 if either node is empty or if there's no connection
-- Returns the transition probability if n2 is a neighbor of n1
-- Returns 1.0 if same node and it's absorbing (stays in place)
getProbabilityOfTransition _ EmptyNode = 0.0
getProbabilityOfTransition EmptyNode _ = 0.0
getProbabilityOfTransition (Node _ pos1 prob neighbors) n2@(Node _ pos2 _ _)
  | pos2 `Data.List.elem` neighbors = prob
  | pos1 == pos2 = fromIntegral $ isAbsorbingState n2
  | otherwise = 0.0

-- | Finds the node corresponding to the starting position (Initial cell)
getStartingPositionNode (ProbabilityGraph _ nodes) = 
  let  isInitialNode = (\case {(Node Initial _ _ _) -> True; _ -> False})
  in Data.List.head $ Data.List.filter isInitialNode nodes 

-- | Finds the index of a node in a sequence
-- Returns -1 if node not found
indexOf node nodes = let newSeq = Seq.takeWhileL (/= node)  nodes
                     in if Seq.length newSeq == Seq.length nodes then -1 else Seq.length newSeq

-- | Calculates the fundamental matrix N = (I - Q)^(-1)
-- This gives expected number of visits to each transient state
-- Used in absorbing Markov chain analysis
getFundamentalMatrix m = inverseGaussJordan ((buildIdentityMatrix m) `subtractMatrix` m)

-- | Main function: Calculates probability of reaching an exit from start
-- Uses absorbing Markov chain analysis:
-- 1. Partition nodes into absorbing (first) and transient (last)
-- 2. Extract Q (transient→transient) and R (transient→absorbing) submatrices
-- 3. Calculate fundamental matrix N = (I-Q)^(-1)
-- 4. Calculate absorption probabilities B = N × R
-- 5. Sum probabilities for all Exit states in the starting row of B
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
                           fr =  (\m -> m `multiply` r) <$> (getFundamentalMatrix q)
                    in  maybe 0
                            (\ matrix ->    fromRational $
                                              Data.Foldable.sum $
                                              Seq.mapWithIndex (\ _ (_,p) -> p) $
                                              Seq.filter (\case {(Node Exit _ _ _,_) -> True; _ -> False})  $
                                              Seq.zip (Seq.take countAbsorvingStates sortedNodes)
                                                      (Seq.take countAbsorvingStates (getRow (startingNodeIndex - countAbsorvingStates) matrix))) fr

-- | Extracts the list of neighbor positions from a node
getNeighbors (Node _ _ _ n) = n

-- | Adds a bidirectional tunnel between two cells in the maze
-- Takes 1-indexed coordinates and converts to 0-indexed
-- Updates both cells to be Tunnel cells pointing to each other
addTunnel (Maze initial cells) i1 j1 i2 j2 =
  let x1 = (i1 -1)
      y1 = (j1 - 1)
      x2 = (i2 - 1)
      y2 = (j2 - 1)
      mazeUpdate :: Int -> Int -> Int -> Int -> Seq (Seq Cell) -> Seq (Seq Cell)
      mazeUpdate x_1 y_1 x_2 y_2 myCells = 
        let row = (Seq.index myCells x_1)
        in Seq.update x_1  (Seq.update y_1 (Tunnel (Position x_2 y_2)  (row `Seq.index` y_1)) row) myCells
  in  Maze initial (mazeUpdate x2 y2 x1 y1 (mazeUpdate x1 y1 x2 y2 cells))

-- | Adds a row of cells to the maze builder
-- If this is the first row containing Initial cell, records the starting position
addRow :: MazeBuilder -> [Cell] -> MazeBuilder
addRow (MazeBuilder startingPos@(Just _) oldCells) newCells = MazeBuilder startingPos (oldCells |> Seq.fromList newCells)
addRow (MazeBuilder Nothing oldCells) newCells =  MazeBuilder ((\ idx -> (Position (Seq.length oldCells) idx)) <$> (Data.List.elemIndex Initial newCells)) (oldCells |> Seq.fromList newCells)

-- | Converts a MazeBuilder to a complete Maze
-- Requires that starting position has been found
toMaze (MazeBuilder (Just position) cells) = Maze position cells

-- | Builds a complete maze from a 2D list of cells
-- Automatically finds and records the starting position
buildMaze :: [[Cell]] -> Maze
buildMaze =  toMaze . Data.List.foldl addRow (MazeBuilder Nothing Seq.empty)

-- | Safely accesses a cell at position (i, j) in the grid
-- Returns Nothing if coordinates are out of bounds
cellAt :: Int -> Int -> Seq (Seq a) -> Maybe a
cellAt i j cells
 | i < 0 = Nothing
 | j < 0 = Nothing
 | i >= Seq.length cells = Nothing
 | j >= Seq.length (cells `Seq.index` 0) = Nothing
 | otherwise = Just ((cells `Seq.index` i) `Seq.index` j)

-- | Checks if the frog can move to position (i, j)
-- Returns Nothing if out of bounds or obstacle
-- Returns Just (i, j, cell) if movement is valid
canMove :: Int -> Int -> Maze -> Maybe (Int, Int, Cell)
canMove i j (Maze _ cells) = case cellAt i j cells of
                     Nothing -> Nothing
                     Just Obstacle -> Nothing
                     (Just cell) -> Just (i,j,cell)

-- | Finds all valid neighboring positions from a given position
-- Handles tunnels by finding neighbors from the tunnel exit
-- For Free/Initial cells: returns 4-directional neighbors (up, down, left, right)
-- For Mine/Exit: returns empty list (absorbing states)
neighbors (Position xi xj)  maze@(Maze _ cells) =
  let
    neighborsIndexes i j = [(i,j-1),(i,j+1),(i-1,j),(i+1,j)]
    myNeighbors i j = Data.Foldable.concatMap (\ (ni,nj) -> Data.Maybe.maybeToList (canMove ni nj maze)) (neighborsIndexes i j)
  in case (Data.Maybe.fromJust (cellAt xi xj cells)) of
        (Tunnel (Position otherX otherY) _) ->  myNeighbors otherX otherY
        Free -> myNeighbors xi xj 
        Initial -> myNeighbors xi xj                                      
        Mine -> []
        Exit -> []

-- | Traverses the entire maze from the starting position
-- Returns list of all reachable positions (nodes in the graph)
-- Uses depth-first search with visited set to avoid cycles
traverseMaze maze@(Maze initial cells) =
  let
     traverseMazeRec:: (Set Position) -> Position -> [Position]
     traverseMazeRec visited current =
        let
          pendingNeighbors =  Data.List.filter (\ p -> not (Set.member p visited)) $ Data.List.map (\ (ni,nj,_) -> (Position ni nj)) $ neighbors current maze
          newVisited = (visited `Set.union` (Set.fromList pendingNeighbors))
        in current:(Data.Foldable.concatMap (traverseMazeRec newVisited) pendingNeighbors)
 in nub $ traverseMazeRec (Set.singleton initial) initial

-- | Builds a probability graph (Markov chain) from a maze
-- Returns EmptyGraph if maze is empty or has no exits
-- For each reachable cell, creates a Node with:
-- - Cell type
-- - Position
-- - Transition probability (1/n where n = number of neighbors)
-- - List of neighbor positions
buildProbabilitiesGraph EmptyMaze = EmptyGraph
buildProbabilitiesGraph maze@(Maze start cells)
 | not (hasExits maze) = EmptyGraph
 | True = let
           neighborsIndexes i j = [(i,j-1),(i,j+1),(i-1,j),(i+1,j)]
           neighbors i j  = Data.Foldable.concatMap (\ (ni,nj) -> Data.Maybe.maybeToList (canMove ni nj maze)) (neighborsIndexes i j)
           nodesToProcess = traverseMaze maze
           buildProbabilitiesNode :: Int -> Int -> Cell -> Node
           buildProbabilitiesNode i j cell  =  case cell of
                                              Mine -> Node Mine (Position i j) 0.0 []
                                              tunnel@(Tunnel (Position ni nj) oldCell) -> let ns = neighbors ni nj
                                                                                              nsLength = Data.List.length ns
                                                                                              neighborsProb = if nsLength == 0 then 0 else 1.0 / fromIntegral nsLength
                                                                                           in Node oldCell (Position i j) neighborsProb (Data.List.map (\ (x,y,_) -> Position x y) ns)
                                              Obstacle -> EmptyNode
                                              Exit -> Node Exit (Position i j) 0.0 []
                                              theCell -> let ns = neighbors i j
                                                             nsLength = Data.List.length ns
                                                             neighborsProb = if nsLength == 0 then 0 else 1.0 / fromIntegral nsLength
                                                         in Node theCell (Position i j) neighborsProb (Data.List.map (\ (x,y,_) -> Position x y) ns)
        in
         ProbabilityGraph start $ Data.List.map (\ (Position i j) -> let (Just cell) = cellAt i j cells in buildProbabilitiesNode i j cell) nodesToProcess

-- | Converts a character to its corresponding Cell type
-- 'A' -> Initial (starting position)
-- 'O' -> Free (open cell)
-- '*' -> Mine (death)
-- '%' -> Exit (goal)
-- '#' -> Obstacle (impassable)
newCell :: Char -> Cell
newCell c =
  case c of
       'A' -> Initial
       'O' -> Free
       '*' -> Mine
       '%' -> Exit
       '#' -> Obstacle
