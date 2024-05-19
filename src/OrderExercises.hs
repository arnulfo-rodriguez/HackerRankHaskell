module OrderExercises (orderExercisesMain) where

import qualified Data.List as List
import Data.Foldable as Foldable
import Control.Monad as Monad
import System.IO
import Data.Sequence as Seq

data SegmentSum = SegmentSum { start :: Int, end :: Int, total :: Int } deriving Eq

instance Ord SegmentSum where
  (SegmentSum s1 e1 t1) < (SegmentSum s2 e2 t2) = t1 < t2 || (t1 == t2 && s1 < s2) || (t1 == t2 && s1 == s2 && e1 < e2)
  (SegmentSum s1 e1 t1) <= (SegmentSum s2 e2 t2) = t1 <= t2 || (t1 == t2 && s1 <= s2) || (t1 == t2 && s1 == s2 && e1 <= e2)

add (SegmentSum s1 e1 t1) seg2@(SegmentSum s2 e2 t2)
 | e1 + 1 == s2 = SegmentSum s1 e2 (t1 + t2)
 | (minBound::Int) == e1 = seg2

accumulateStep :: (SegmentSum, SegmentSum) -> SegmentSum -> (SegmentSum, SegmentSum)
accumulateStep (maxGlobalSubArray, maxSumCurrentSubarray) currentSegment =
   let maxEndingHere = max currentSegment (add maxSumCurrentSubarray currentSegment)
   in (max maxEndingHere maxGlobalSubArray, maxEndingHere)

kadaneAlgorithm :: Seq Int -> Maybe SegmentSum
kadaneAlgorithm Empty = Nothing
kadaneAlgorithm arr =
  let
    nullSegment = SegmentSum (minBound::Int) (minBound::Int) (minBound::Int)
    (result,_) = Foldable.foldl accumulateStep (nullSegment, nullSegment)  $ Seq.mapWithIndex (\ idx value-> SegmentSum idx idx value) arr
  in Monad.guard (total result > 0) >> return result

getSubarrays :: Seq a -> SegmentSum -> (Seq a, Seq a)
getSubarrays xs (SegmentSum i j _) =
  let (before, rest) = Seq.splitAt i xs
      (_, after) = Seq.splitAt (j - i + 1) rest
  in (before, after)

orderExercises :: Seq Int -> [Int]
orderExercises Empty = []
orderExercises arr =
  let result = kadaneAlgorithm arr
  in maybe [] (\s -> let (leftArray, rightArray) = getSubarrays arr s
                     in total s : mergeDescending (orderExercises leftArray) (orderExercises rightArray)) result


mergeDescending :: [Int] -> [Int] -> [Int]
mergeDescending [] ys = ys
mergeDescending xs [] = xs
mergeDescending (x:xs) (y:ys)
  | x >= y = x : mergeDescending xs (y:ys)
  | otherwise = y : mergeDescending (x:xs) ys

orderExercisesMain :: IO()
orderExercisesMain = do
  firstLineStr <- System.IO.getLine
  let [_ , t] = map read (words firstLineStr)
  secondLineStr <- System.IO.getLine
  let theList = Seq.fromList $ List.map (\x -> read x ::Int) $ words secondLineStr
  let result = List.take t $ orderExercises theList
  mapM_ print result
