module InsertionSort(
 insertionSort
) where

import Data.Sequence as Seq
import Data.List as List
import Data.Traversable as Traversable
import Data.Foldable as Foldable
  
merge :: Seq Int -> Seq Int -> (Int, Seq Int) -> (Int, Seq Int)
merge Seq.Empty b (currentCount, currentMerge) = (currentCount + Seq.length b * Seq.length currentMerge, currentMerge)
merge a Seq.Empty (currentCount, currentMerge) = (currentCount, currentMerge >< a)
merge left@(leftHead :<| leftTail) right@(rightHead :<| rightTail) (currentCount, currentMerge)
 | leftHead <= rightHead = merge leftTail right (currentCount, currentMerge |> leftHead)
 | otherwise = merge left rightTail (currentCount + Seq.length left, currentMerge |> rightHead)
     
mergeSort :: Seq Int -> (Int, Seq Int)
mergeSort Seq.Empty = (0, Seq.empty)
mergeSort s@(_ :<| Seq.Empty) = (0, s)
mergeSort theSeq =
  let
    middle = Seq.length theSeq `div` 2
    (left,right) = Seq.splitAt middle theSeq
    (leftCount, leftSorted) = mergeSort left
    (rightCount, rightSorted) =  mergeSort right
    (mergeCount, merged) =  merge leftSorted rightSorted (0, Seq.empty)
  in (rightCount + leftCount + mergeCount, merged)

insertionSort arr =
  let
    (count, _) = mergeSort (Seq.fromList arr)
  in count