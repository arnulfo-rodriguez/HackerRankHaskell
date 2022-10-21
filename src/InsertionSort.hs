module InsertionSort(
 insertionSort
) where

import Data.Sequence as Seq
import Data.List as List
import Data.Traversable as Traversable
import Data.Foldable as Foldable
import Data.Array as Array

data Segment = Segment Int Int (Array Int Int) deriving (Show)

isEmptySegment (Segment leftBounds rightBounds _) = leftBounds > rightBounds
segmentLength (Segment leftBounds rightBounds _) = (rightBounds - leftBounds) + 1

mySplitAt ix (Segment leftBounds rightBounds array) =
  let theRealIX = leftBounds + ix
  in (Segment leftBounds (theRealIX -1) array, Segment theRealIX rightBounds array)

toSequence (Segment leftBounds rightBounds array) = Seq.fromList $ List.map (array Array.!) [leftBounds.. rightBounds]

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


segmentMergeSort :: Segment -> (Int, Seq Int)
segmentMergeSort seg
 | isEmptySegment seg = (0, Seq.empty)
 | segmentLength seg == 1 = (0, toSequence seg)
 | otherwise =
   let
     middle = segmentLength seg `div` 2
     (left,right) = mySplitAt middle seg
     (leftCount, leftSorted) = segmentMergeSort left
     (rightCount, rightSorted) =  segmentMergeSort right
     (mergeCount, merged) =  merge leftSorted rightSorted
   in (rightCount + leftCount + mergeCount, merged)

insertionSort len lst =
  let
    bounds@(lower,upper) = (0,len -1)
    arr = Array.listArray bounds lst
    (count, _) = segmentMergeSort (Segment lower upper arr)
  in count