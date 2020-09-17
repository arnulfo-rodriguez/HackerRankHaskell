module MorganAndString where

import Data.List

morganAndString1 a [] = a
morganAndString1 [] a = a
morganAndString1 l1@(a : rest1) l2@(b : rest2)
  | a < b = a : morganAndString1 rest1 l2
  | b < a = b : morganAndString1 l1 rest2
  | otherwise
  = let
      withA = a : morganAndString1 rest1 l2
      withB = b : morganAndString1 l1 rest2
    in if withA < withB then withA else withB


data MASNode = Node Moves String String String
  deriving (Show)
  


converge (Node (Moves dA1 dB1 _) _ _ _) (Node (Moves dA2 dB2 _) _ _ _ ) = dA1 == dA2 && dB1 == dB2


nextMove (Moves depthA depthB  []) move@PickA = Moves (depthA + 1) depthB [move]
nextMove (Moves depthA depthB [a]) move@PickA = Moves (depthA + 1) depthB [move,a]
nextMove (Moves depthA depthB [a,b]) move@PickA = Moves (depthA + 1) depthB [move,a]
nextMove (Moves depthA depthB []) move@PickB = Moves depthA (depthB + 1) [move]
nextMove (Moves depthA depthB [a]) move@PickB = Moves depthA (depthB + 1) [move,a]
nextMove (Moves depthA depthB [a,b]) move@PickB = Moves depthA (depthB + 1) [move,a]


firstOrEmpty [] = []
firstOrEmpty (a:_) = [a]

data Move = PickA | PickB
  deriving (Show,Eq)
data Moves = Moves Int Int [Move]
  deriving Show

compareByAccumulated (Node _ _ str1 str2) (Node _ _ str3 str4) 
  | min (firstOrEmpty str1)  (firstOrEmpty str2) == min (firstOrEmpty str3) (firstOrEmpty str4) = EQ 
  | min (firstOrEmpty str1)  (firstOrEmpty str2) < min (firstOrEmpty str3) (firstOrEmpty str4) = LT 
  | otherwise = GT

next (Node moves str [] []) = []
next (Node moves str l1 []) = [Node (nextMove moves PickA) (str ++ l1) [] []]
next (Node moves str [] l2) = [Node (nextMove moves PickB) (str ++ l2) [] []]
next (Node moves str l1@(a1:rest1) l2@(a2:rest2)) = let nextA1 = Node (nextMove moves PickA) (str ++ [a1]) rest1 l2
                                                        nextA2 = Node (nextMove moves PickB) (str ++ [a2]) l1 rest2
                                              in if a1 < a2 then
                                                  [nextA1]
                                                 else if a2 < a1 then
                                                  [nextA2]
                                                 else
                                                   [nextA1,nextA2]

topMin [] (a:rest) = topMin [a] rest
topMin l [] = l
topMin l@(firstMin:_) (remainingHead:rest) = if compareByAccumulated firstMin remainingHead == EQ then
                                                 topMin (remainingHead:l) rest
                                             else
                                                 l

merge []  node = [node]
merge l@(head:rest) node  = if converge node head then
                              l
                           else head:merge rest node

mergeAll = foldl merge []
     
morganAndStringRec nodes@(h:_) = case mergeAll (concatMap next  (topMin [] $ sortBy compareByAccumulated nodes)) of
                              [] -> case h of
                                      (Node _ str [] []) -> nodes
                              nextGen -> morganAndStringRec nextGen
