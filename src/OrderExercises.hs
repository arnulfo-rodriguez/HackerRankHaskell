{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module OrderExercises where

import qualified Data.List as List
import Data.Foldable as Foldable

data SegmentSum = SegmentSum { start :: Int, end :: Int, total :: Int }

instance Ord SegmentSum where
  
  
accumulateStep (maxGlobalSubArray, maxSumCurrentSubarray) currentSegment =
   let maxEndingHere = max currentValue (maxSumCurrentSubarray + currentValue)
   in (max maxEndingHere maxGlobalSubArray, maxEndingHere)

kadaneAlgorithm k arr =
  let (result,_) = List.foldl accumulateStep (minBound::Int,0) $ List.zipWith (\ (idx,value)-> SegmentSum idx idx value)  [0..] arr
  in result