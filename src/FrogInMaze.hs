module FrogInMaze(
  Position(..),
  Cell(..),
  Maze(..),
  newCell,
  addTunnel,
  addRow,
  buildMaze
) where

import  Data.Sequence as Seq
import Data.List

data Position = Position Int Int deriving(Eq,Show)
data Cell = Free  | Mine  | Obstacle  | Exit  | Initial  | Tunnel Position  deriving(Eq,Show)
data Maze = EmptyMaze | Maze (Seq (Seq Cell))  deriving(Eq,Show)

addTunnel :: Maze -> Int -> Int -> Int -> Int -> Maze
addTunnel (Maze cells) i1 j1 i2 j2 =
  let x1 = (i1 -1)
      y1 = (j1 - 1)
      x2 = (i2 - 1)
      y2 = (j2 - 1)
      mazeUpdate :: Int -> Int -> Int -> Int -> Seq (Seq Cell) -> Seq (Seq Cell)
      mazeUpdate x_1 y_1 x_2 y_2 myCells = Seq.update x_1  (Seq.update y_1 (Tunnel (Position x_2 y_2)) (Seq.index myCells x_1)) myCells
  in  Maze (mazeUpdate x2 y2 x1 y1 (mazeUpdate x1 y1 x2 y2 cells))

addRow :: Maze -> [Cell] -> Maze
addRow EmptyMaze cells = Maze (Seq.singleton (Seq.fromList cells))
addRow (Maze oldCells) newCells = Maze (oldCells |> Seq.fromList newCells)

buildMaze :: [[Cell]] -> Maze
buildMaze = Data.List.foldl addRow EmptyMaze

newCell :: Char -> Cell
newCell c =
  case c of
       'A' -> Initial
       'O' -> Free
       '*' -> Mine
       '%' -> Exit
       '#' -> Obstacle
