{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  ( nonDivisibleSubset
  ) where

import           Data.Foldable
import           Data.Monoid

data IntArraySet
  = EmptyArraySet
  | NonEmptyArraySet [Int] IntArraySet
  | MergedArraySet IntArraySet IntArraySet
  | FoldedArraySet Int IntArraySet

allSubsetOfSize :: Int -> [Int] -> [[Int]]
allSubsetOfSize size l@(head:rest)
  | size == 0 = []
  | length l == size = [l]
  | length l > size =
    let setsWithoutHeadOfSizeN = allSubsetOfSize size rest
        setsWithHeadOfSizeN = map (\subset -> head : subset) $ allSubsetOfSize (size - 1) rest
     in setsWithoutHeadOfSizeN ++ setsWithHeadOfSizeN
  | otherwise = []

allSubsets l = concatMap (`allSubsetOfSize` l) (enumFromThenTo (length l) (length l - 1) 1)

allPairs []      = []
allPairs [x]     = []
allPairs (h:j:t) = (h, j) : (allPairs (h : t) ++ allPairs (j : t))

pairsNotDivisibleBy :: Int -> [(Int, Int)] -> Bool
pairsNotDivisibleBy k [] = True
pairsNotDivisibleBy k ((a, b):rest) = (((a + b) `mod` k) > 0) && pairsNotDivisibleBy k rest

nonDivisibleSubset k s =
  let subsets = allSubsets s
      nonDivisibleSubsets = filter (pairsNotDivisibleBy k . allPairs) subsets
   in case nonDivisibleSubsets of
        (a:_) -> length a
        _ -> -1
