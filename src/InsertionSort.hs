module InsertionSort(
 insertionSort
) where

import Data.Sequence as Seq
import Data.List as List
import Data.Traversable as Traversable
import Data.Foldable as Foldable
  
data InsertionSortList a = InsertionSortList Int (Seq a)  deriving Show

binarySearchInsert :: Ord a => InsertionSortList a -> a -> InsertionSortList a
binarySearchInsert (InsertionSortList 0 Seq.Empty) newElem = InsertionSortList 0 (Seq.singleton newElem)
binarySearchInsert (InsertionSortList count l) newElem =
  let
    insertionSortTmp arr@(first :<| _) 
      | (newElem < first) = (Seq.length arr, newElem <| arr)
    insertionSortTmp arr@(_ :|> last)
      | (newElem >= last) = (0,arr |> newElem)
    insertionSortTmp arr =
      let 
        midIndex = (Seq.length arr `div` 2)
        lists = Seq.splitAt midIndex arr
      in case lists of
            (prefix, suffix@(elem :<| Seq.Empty)) 
              | newElem >= elem -> (0, (prefix >< suffix) |> newElem)
              | Seq.length prefix == 0 -> (1, newElem <| suffix)   
            (prefix, suffix@(elem :<| _)) -> if newElem < elem then 
                                                case insertionSortTmp prefix of
                                                  (nextCount,newPrefix) -> (Seq.length suffix + nextCount, newPrefix >< suffix)
                                             else
                                                case insertionSortTmp suffix of
                                                  (nextCount,newSuffix) -> (nextCount,prefix >< newSuffix)                                          
  in case insertionSortTmp l of
        (newCount,newSeq) -> InsertionSortList (newCount + count) newSeq


findIndexLog:: Seq Int -> Int -> Int -> Int -> Int
findIndexLog s leftBoundary rightBoundary value
  | leftBoundary + 1 == rightBoundary = case (s `Seq.index` leftBoundary, s `Seq.index` rightBoundary) of
                                          (leftValue,rightValue) 
                                            | value > leftValue && value <= rightValue -> rightBoundary
                                            | value > rightValue  -> rightBoundary + 1
                                            | otherwise -> leftBoundary
  | leftBoundary >= rightBoundary = rightBoundary
  | otherwise = let 
                   middle = (leftBoundary + rightBoundary) `div` 2
                   valueAtMiddle = s `Seq.index` middle
                   (newLeftBoundary,newRightBoundary) = if value > valueAtMiddle then (middle,rightBoundary) else (leftBoundary,middle)
                in findIndexLog s newLeftBoundary newRightBoundary value  
      

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