{-|
Module      : AbsolutePermutation
Description : Solution for the Absolute Permutation problem from HackerRank

== Problem Statement

Given a positive integer 'n' and a non-negative integer 'k', find a permutation 
of the first 'n' positive integers where the absolute difference between each 
element and its position is exactly 'k'.

That is, for permutation P[1..n], we need: |P[i] - i| = k for all i ∈ [1..n]

== Solution Approach

The solution uses a pattern-based approach:

1. Base case (k=0): The identity permutation [1..n] satisfies |i - i| = 0

2. Impossibility check: If n is not divisible by 2k, no valid permutation exists.
   Why? Elements must swap in pairs separated by distance k, requiring groups 
   of 2k elements to maintain the constraint.

3. Pattern generation (k>0, n divisible by 2k):
   - Divide [1..n] into chunks of size 2k
   - Within each 2k chunk, split into two k-sized sub-chunks
   - Reverse the order of these two sub-chunks
   
   Example: n=6, k=2
   Original: [1,2,3,4,5,6]
   Chunks:   [[1,2],[3,4]] | [[5,6],...]
   Reversed: [[3,4],[1,2]] | [[5,6],...]
   Result:   [3,4,1,2,5,6]
   
   Verification: |3-1|=2, |4-2|=2, |1-3|=2, |2-4|=2, etc.

== Implementation

Uses Data.Sequence for efficient chunking operations:
- chunksOf (2*k): Splits into 2k-sized blocks
- chunksOf k: Splits each block into k-sized sub-chunks  
- Seq.reverse: Reverses the two sub-chunks within each block
- join: Flattens the nested sequences back to a single list

Time Complexity: O(n)

-}
module AbsolutePermutation where


import Data.Sequence as Seq
import Data.Foldable as Foldable
import Control.Monad
  
absolutePermutation n 0 = [1..n]
absolutePermutation n k
 | (n `mod` (k * 2)) /= 0 = [-1]
 | otherwise = let chunks = Seq.chunksOf (2*k) (Seq.fromList [1..n])
                   reverseSubChunk _ chunk =  Control.Monad.join (Seq.reverse (Seq.chunksOf k chunk))
               in  Foldable.toList $  Control.Monad.join (Seq.mapWithIndex reverseSubChunk chunks)