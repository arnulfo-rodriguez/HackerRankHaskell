{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module OrderExercises where

import qualified Data.List as List
import Data.Foldable as Foldable
import Control.Monad as Monad

data SegmentSum = SegmentSum { start :: Int, end :: Int, total :: Int } deriving Eq

instance Ord SegmentSum where
  (SegmentSum s1 e1 t1) < (SegmentSum s2 e2 t2) = t1 < t2 || (t1 == t2 && s1 < s2) || (t1 == r2 && s1 == s2 && e1 < e2)
  (SegmentSum _ _ t1) <= (SegmentSum _ _ t2) = t1 <= t2 || (t1 == t2 && s1 <= s2) || (t1 == r2 && s1 == s2 && e1 <= e2)

add (SegmentSum s1 e1 t1) seg2@(SegmentSum s2 e2 t2)
 | e1 + 1 == s2 = (SegmentSum s1 e2 (t1 + t2))
 | (minBound::Int) == e1 = seg2

accumulateStep :: (SegmentSum, SegmentSum) -> SegmentSum -> (SegmentSum, SegmentSum)
accumulateStep (maxGlobalSubArray, maxSumCurrentSubarray) currentSegment =
   let maxEndingHere = max currentSegment (add maxSumCurrentSubarray currentSegment)
   in (max maxEndingHere maxGlobalSubArray, maxEndingHere)

kadaneAlgorithm :: [Int] -> Maybe SegmentSum
kadaneAlgorithm [] = Nothing
kadaneAlgorithm arr =
  let
    nullSegment = (SegmentSum (minBound::Int) (minBound::Int) (minBound::Int))
    (result,_) = List.foldl accumulateStep (nullSegment, nullSegment)  $ List.zipWith (\ idx value-> SegmentSum idx idx value)  [0..] arr
  in Monad.guard (total result > 0) >> return result

getSubarrays xs (SegmentSum i j _) =
  let (before, rest) = splitAt i xs
      (_, after) = splitAt (j - i + 1) rest
  in (before, after)

orderExercises :: Int -> [Int] -> [Int]
orderExercises 0 _ = []
orderExercises k arr =
  let result = kadaneAlgorithm arr
  in maybe [] (\s -> let (leftArray, rightArray) = getSubarrays arr s
                     in (total s) : orderExercises (k - 1) leftArray ++ orderExercises (k - 1) rightArray) result

