{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
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

data Position = Position Int Int deriving(Eq,Show,Ord)
data Cell = Free  | Mine  | Obstacle  | Exit  | Initial  | Tunnel Position  deriving(Eq,Show)
data MazeBuilder = MazeBuilder (Maybe Position) (Seq (Seq Cell))
data Maze = EmptyMaze | Maze Position (Seq (Seq Cell))  deriving(Eq,Show)
data Node = EmptyNode | Node Cell Double [Position] deriving(Show)
data ProbabilityGraph = EmptyGraph | ProbabilityGraph Position (Seq (Seq Node)) deriving(Show)

getNeighbors (Node _ _ n) = n

addTunnel :: Maze -> Int -> Int -> Int -> Int -> Maze
addTunnel (Maze initial cells) i1 j1 i2 j2 =
  let x1 = (i1 -1)
      y1 = (j1 - 1)
      x2 = (i2 - 1)
      y2 = (j2 - 1)
      mazeUpdate :: Int -> Int -> Int -> Int -> Seq (Seq Cell) -> Seq (Seq Cell)
      mazeUpdate x_1 y_1 x_2 y_2 myCells = Seq.update x_1  (Seq.update y_1 (Tunnel (Position x_2 y_2)) (Seq.index myCells x_1)) myCells
  in  Maze initial (mazeUpdate x2 y2 x1 y1 (mazeUpdate x1 y1 x2 y2 cells))

addRow :: MazeBuilder -> [Cell] -> MazeBuilder
addRow (MazeBuilder startingPos@(Just _) oldCells) newCells = MazeBuilder startingPos (oldCells |> Seq.fromList newCells)
addRow (MazeBuilder Nothing oldCells) newCells = case Data.List.elemIndex Initial newCells of
                                                    Nothing -> MazeBuilder Nothing (oldCells |> Seq.fromList newCells)
                                                    Just idx -> MazeBuilder (Just (Position (Seq.length oldCells) idx)) (oldCells |> Seq.fromList newCells)

toMaze (MazeBuilder (Just position) cells) = Maze position cells

buildMaze :: [[Cell]] -> Maze
buildMaze =  toMaze . (Data.List.foldl addRow (MazeBuilder Nothing Seq.empty))
cellAt :: Int -> Int -> (Seq (Seq a)) -> Maybe a
cellAt i j cells
 | i < 0 = Nothing
 | j < 0 = Nothing
 | i >= Seq.length cells = Nothing
 | j >= Seq.length (cells `Seq.index` 0) = Nothing
 | otherwise = Just ((cells `Seq.index` i) `Seq.index` j)

canMove :: Int -> Int -> Maze -> Maybe (Int, Int, Maybe Cell)
canMove i j (Maze _ cells) = case cellAt i j cells of
                     Nothing -> Nothing
                     Just Obstacle -> Nothing
                     x -> Just (i,j,x)

buildProbabilitiesGraph :: Maze -> ProbabilityGraph
buildProbabilitiesGraph EmptyMaze = EmptyGraph
buildProbabilitiesGraph maze@(Maze start cells) =
  let 
     neighborsIndexes i j = [(i,j-1),(i,j+1),(i-1,j),(i+1,j)]
     neighbors i j  = Data.List.concatMap (\ (ni,nj) -> (Data.Maybe.maybeToList (canMove ni nj maze))) (neighborsIndexes i j)
     buildProbabilitiesNode :: Int -> Int -> Cell -> Node
     buildProbabilitiesNode i j cell  =  case cell of
                                        Mine -> Node Mine 0.0 []
                                        tunnel@(Tunnel (Position ni nj)) -> let ns = (neighbors ni nj)
                                                                                    in Node tunnel (1.0 / (fromIntegral (Data.List.length ns))) (Data.List.map (\ (x,y,_) -> ((Position x y))) ns)
                                        Obstacle -> EmptyNode
                                        Exit -> Node Exit 0.0 []
                                        theCell -> let ns = (neighbors i j)
                                                         in Node theCell (1.0 / (fromIntegral (Data.List.length ns))) (Data.List.map (\ (x,y,_) -> ((Position x y))) ns)
  in
   ProbabilityGraph start $ Seq.mapWithIndex (\ i row -> Seq.mapWithIndex (\ j cell -> buildProbabilitiesNode i j cell) row) cells

fromStartToExit :: ProbabilityGraph -> Double
fromStartToExit (ProbabilityGraph start cells) =
  let
    fromHereToExitRec :: Position -> Set Position -> Double
    fromHereToExitRec (Position i j) visited = case cellAt i j cells of
                                       (Just (Node Exit _ _)) ->  1.0
                                       (Just (Node Mine _ _)) -> 0.0
                                       (Just (Node n probability neighbors)) -> let notVisited = Set.filter (\ e -> Set.notMember e visited) (Set.fromList neighbors)
                                                                                    newVisited = notVisited `Set.union` visited
                                                                                    neighborsProbabilities = Set.map (\ p -> probability * (fromHereToExitRec p newVisited)) notVisited
                                                                                 in Data.Foldable.sum neighborsProbabilities
   in (fromHereToExitRec start (Set.singleton start))

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

main :: IO()
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
                    
    putStrLn $ show $ fromStartToExit $ buildProbabilitiesGraph $ Data.List.foldl (\ m (i1,j1,i2,j2) -> addTunnel m i1 j1 i2 j2) maze tunnels

