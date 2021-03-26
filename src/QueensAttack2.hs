module QueensAttack2
  ( Pos(..)
  , Board(..)
  , Queen(..)
  , queensAttack
  ) where

import           Data.List (foldl, groupBy, sortBy)

data Pos =
  Pos Int Int
  deriving (Eq, Show)

data Board =
  Board Queen Int [Pos]
  deriving (Show)

newtype Queen =
  Queen Pos
  deriving (Show)

data Direction
  = Up
  | Down
  | MyLeft
  | MyRight
  | DiagonalUpRight
  | DiagonalDownRight
  | DiagonalUpLeft
  | DiagonalDownLeft
  deriving (Eq, Show, Ord)

data Distance =
  Distance Direction Int
  deriving (Show)

unblockedQueenDistanceUp (Board (Queen (Pos x _)) size _) = size - x

unblockedQueenDistanceDown (Board (Queen (Pos x _)) size _) = x - 1

unblockedQueenDistanceLeft (Board (Queen (Pos _ y)) size _) = y - 1

unblockedQueenDistanceRight (Board (Queen (Pos _ y)) size _) = size - y

unblockedQueenDistanceDiagonalUpRight board = min (unblockedQueenDistanceRight board) (unblockedQueenDistanceUp board)

unblockedQueenDistanceDiagonalDownRight board =
  min (unblockedQueenDistanceRight board) (unblockedQueenDistanceDown board)

unblockedQueenDistanceDiagonalUpLeft board = min (unblockedQueenDistanceLeft board) (unblockedQueenDistanceUp board)

unblockedQueenDistanceDiagonalDownLeft board = min (unblockedQueenDistanceLeft board) (unblockedQueenDistanceDown board)

directions =
  [ (Up, unblockedQueenDistanceUp)
  , (Down, unblockedQueenDistanceDown)
  , (MyLeft, unblockedQueenDistanceLeft)
  , (MyRight, unblockedQueenDistanceRight)
  , (DiagonalUpRight, unblockedQueenDistanceDiagonalUpRight)
  , (DiagonalUpLeft, unblockedQueenDistanceDiagonalUpLeft)
  , (DiagonalDownRight, unblockedQueenDistanceDiagonalDownRight)
  , (DiagonalDownLeft, unblockedQueenDistanceDiagonalDownLeft)
  ]

isBlocked :: Pos -> [Pos] -> Bool
isBlocked p1 = foldr (\p2 -> (||) (p1 == p2)) False

distanceFromBlockedToQueenUp (Pos x y) (Queen (Pos qx qy)) =
  if (x > qx) && (y == qy)
    then Just $ Distance Up (x - qx - 1)
    else Nothing

distanceFromBlockedToQueenDown (Pos x y) (Queen (Pos qx qy)) =
  if (x < qx) && (y == qy)
    then Just $ Distance Down (qx - x - 1)
    else Nothing

distanceFromBlockedToQueenLeft (Pos x y) (Queen (Pos qx qy)) =
  if (x == qx) && (y < qy)
    then Just $ Distance MyLeft (qy - y - 1)
    else Nothing

distanceFromBlockedToQueenRight (Pos x y) (Queen (Pos qx qy)) =
  if (x == qx) && (y > qy)
    then Just $ Distance MyRight (y - qy - 1)
    else Nothing

distanceFromBlockedToQueenDiagonalUpRight (Pos x y) (Queen (Pos qx qy)) =
  if (x > qx) && (y > qy) && (x - qx) == (y - qy)
    then Just $ Distance DiagonalUpRight (x - qx - 1)
    else Nothing

distanceFromBlockedToQueenDiagonalUpLeft (Pos x y) (Queen (Pos qx qy)) =
  if (x > qx) && (y < qy) && (x - qx) == (qy - y)
    then Just $ Distance DiagonalUpLeft (x - qx - 1)
    else Nothing

distanceFromBlockedToQueenDiagonalDownRight (Pos x y) (Queen (Pos qx qy)) =
  if (x < qx) && (y > qy) && (qx - x) == (y - qy)
    then Just $ Distance DiagonalDownRight (qx - x - 1)
    else Nothing

distanceFromBlockedToQueenDiagonalDownLeft (Pos x y) (Queen (Pos qx qy)) =
  if (x < qx) && (y < qy) && (qx - x) == (qy - y)
    then Just $ Distance DiagonalDownLeft (qx - x - 1)
    else Nothing

distance q blocked [] = []
distance q blocked (distanceFunc:distanceFuncs) =
  case distanceFunc blocked q of
    Just d  -> [d]
    Nothing -> distance q blocked distanceFuncs

distances :: Queen -> [Pos] -> [Pos -> Queen -> Maybe Distance] -> [Distance]
distances q blocked funcs = concatMap (\b -> distance q b funcs) blocked

minDistance (Distance x1 l1) (Distance x2 l2) =
  if l1 < l2
    then Distance x1 l1
    else Distance x2 l2

minPerDirection :: [Distance] -> Maybe Distance
minPerDirection []       = Nothing
minPerDirection (d:rest) = Just $ Data.List.foldl minDistance d rest

minimumDistances (Board q _ blocked) funcs =
  let allDistances = distances q blocked funcs
      distancesByDirection =
        groupBy (\(Distance dirA _) (Distance dirB _) -> dirA == dirB) $
        sortBy (\(Distance dirA _) (Distance dirB _) -> compare dirA dirB) allDistances
      minsPerDirection =
        concatMap
          (\d ->
             case minPerDirection d of
               Nothing -> []
               Just x  -> [x])
          distancesByDirection
   in minsPerDirection

queensAttack b =
  let md =
        minimumDistances
          b
          [ distanceFromBlockedToQueenUp
          , distanceFromBlockedToQueenDown
          , distanceFromBlockedToQueenLeft
          , distanceFromBlockedToQueenRight
          , distanceFromBlockedToQueenDiagonalUpRight
          , distanceFromBlockedToQueenDiagonalUpLeft
          , distanceFromBlockedToQueenDiagonalDownRight
          , distanceFromBlockedToQueenDiagonalDownLeft
          ]
   in sum $
      map
        (\(dir, defaultDistance) ->
           case filter (\(Distance d1 _) -> d1 == dir) md of
             []               -> defaultDistance b
             [Distance d len] -> len)
        directions
