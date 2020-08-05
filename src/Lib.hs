module Lib
  ( Pos(..)
  , Board(..)
  , Queen(..)
  , queensAttack
  ) where

data Pos =
  Pos Int Int
  deriving (Eq, Show)

data Board =
  Board Queen Int [Pos]
  deriving (Show)

newtype Queen =
  Queen Pos
  deriving (Show)

isBlocked :: Pos -> [Pos] -> Bool
isBlocked p1 = foldr (\p2 -> (||) (p1 == p2)) False

leftOf (Pos x y) = Pos x (y - 1)

rightOf (Pos x y) = Pos x (y + 1)

belowOf (Pos x y) = Pos (x - 1) y

topOf (Pos x y) = Pos (x + 1) y

upRightOf (Pos x y) = Pos (x + 1) (y + 1)

downRightOf (Pos x y) = Pos (x - 1) (y + 1)

upLeftOf (Pos x y) = Pos (x + 1) (y - 1)

downLeftOf (Pos x y) = Pos (x - 1) (y - 1)

isViable (Pos x y) n = (x > 0) && (x <= n) && (y > 0) && (y <= n)

doMove (Board (Queen pos) size blocked) newPosFunc =
  let newPos = newPosFunc pos
   in if isViable newPos size && not (isBlocked newPos blocked)
        then Just $ Board (Queen newPos) size blocked
        else Nothing

moveLeft b = doMove b leftOf

moveRight b = doMove b rightOf

moveUp b = doMove b topOf

moveDown b = doMove b belowOf

moveUpRight b = doMove b upRightOf

moveUpLeft b = doMove b upLeftOf

moveDownRight b = doMove b downRightOf

moveDownLeft b = doMove b downLeftOf

moves :: Board -> (Board -> Maybe Board) -> [Board]
moves b moveFunc =
  case moveFunc b of
    (Just b1) -> b1 : moves b1 moveFunc
    Nothing -> []

queensAttack b =
  foldl
    (\acc f -> acc + length (moves b f))
    0
    [moveLeft, moveRight, moveUp, moveDown, moveUpRight, moveUpLeft, moveDownRight, moveDownLeft]

