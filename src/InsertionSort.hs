module InsertionSort(
 insertionSort
) where

import Data.Sequence as Seq
import Data.List as List
import Data.Traversable as Traversable
import Data.Foldable as Foldable
  
merge :: Seq Int -> Seq Int -> (Int, Seq Int)
merge Seq.Empty b = (0, b)
merge a Seq.Empty = (0, a)
merge left@(leftHead :<| leftTail) right@(rightHead :<| rightTail) 
 | leftHead <= rightHead = 
   let
    (count,merged)  = merge leftTail right
   in (count, leftHead <| merged)
 | otherwise =
   let 
    (count, merged) = merge left rightTail                             
   in (count + Seq.length left, rightHead <| merged)
     
mergeSort :: Seq Int -> (Int, Seq Int)
mergeSort Seq.Empty = (0, Seq.empty)
mergeSort s@(_ :<| Seq.Empty) = (0, s)
mergeSort theSeq =
  let
    middle = Seq.length theSeq `div` 2
    (left,right) = Seq.splitAt middle theSeq
    (leftCount, leftSorted) = mergeSort left
    (rightCount, rightSorted) =  mergeSort right
    (mergeCount, merged) =  merge leftSorted rightSorted
  in (rightCount + leftCount + mergeCount, merged)

insertionSort arr =
  let
    (count, _) = mergeSort (Seq.fromList arr)
  in count