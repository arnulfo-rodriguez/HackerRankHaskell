{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : NonDivisibleSubSet
Description : Find the maximum subset where no pair sums to a number divisible by k

This module solves the problem: Given a set of integers and a divisor k,
find the size of the largest subset where the sum of any two elements
is not divisible by k.

The solution uses modular arithmetic: two numbers sum to a multiple of k
if and only if their remainders (mod k) sum to k or 0. Therefore, we can
only include one element from each complementary remainder pair (r, k-r).

Special cases:
- Remainder 0: All elements sum to multiples of k, so we can only include one
- Remainder k/2 (when k is even): Elements pair with themselves, so include max one
-}

module NonDivisibleSubSet
  (nonDivisibleSubset
  ) where

import qualified Data.Map.Strict as Map

-- | State tracking remainder frequencies and special cases
-- We track zero and half-divisor remainders separately because:
-- - Zero remainder: elements pair with themselves (x + x ≡ 0 mod k)
-- - Half-divisor remainder: when k is even, x + x ≡ k ≡ 0 mod k
data RemainderState = RemainderState
  { hasZeroRemainder :: Bool  -- ^ At least one element divisible by k
  , hasHalfRemainder :: Bool  -- ^ At least one element with remainder k/2
  , remainderCounts :: Map.Map Int Int  -- ^ Count of elements for each remainder
  } deriving (Show, Eq)

-- | Initial empty state with no remainders
emptyState :: RemainderState
emptyState = RemainderState False False Map.empty

-- | Increment the count for a given remainder in the map
incrementRemainder :: Int -> Map.Map Int Int -> Map.Map Int Int
incrementRemainder remainder = Map.insertWith (+) remainder 1

-- | Add a single element to the remainder state
-- Special remainders (0 and k/2) are tracked as boolean flags,
-- while other remainders are counted in the map
addElementToState :: Int -> RemainderState -> Int -> RemainderState
addElementToState divisor state@(RemainderState hasZero hasHalf counts) element
  | remainder == 0 = state { hasZeroRemainder = True }
  | isHalfDivisor remainder = state { hasHalfRemainder = True }
  | otherwise = state { remainderCounts = incrementRemainder remainder counts }
  where
    remainder = element `mod` divisor
    halfDivisor = divisor `div` 2
    isHalfDivisor r = even divisor && r == halfDivisor

-- | Process all elements and build the complete remainder state
-- Recursively processes the list and accumulates remainder information
buildRemainderState :: Int -> [Int] -> RemainderState
buildRemainderState divisor [] = emptyState
buildRemainderState divisor (element:rest) =
  let nextState = buildRemainderState divisor rest
  in addElementToState divisor nextState element

-- | Calculate the maximum count from complementary remainder pairs
-- For each remainder r in [1, k/2), we can include elements with remainder r
-- OR remainder (k-r), but not both (since r + (k-r) = k ≡ 0 mod k).
-- We choose whichever has more elements.
maxNonDivisibleCount :: Int -> Map.Map Int Int -> Int
maxNonDivisibleCount divisor counts =
  sumMaxPairs 1
  where
    halfDivisor = divisor `div` 2
    
    -- Recursively sum the best choice from each complementary pair
    sumMaxPairs :: Int -> Int
    sumMaxPairs remainder
      | remainder > halfDivisor = 0
      | otherwise = bestPairCount + sumMaxPairs (remainder + 1)
      where
        complementRemainder = divisor - remainder
        countForRemainder = Map.findWithDefault 0 remainder counts
        countForComplement = Map.findWithDefault 0 complementRemainder counts
        -- Choose the remainder that has more elements
        bestPairCount = max countForRemainder countForComplement

-- | Find the maximum size of a subset where no two elements sum to a multiple of k
--
-- Algorithm:
-- 1. Group elements by their remainder when divided by k
-- 2. For complementary remainders (r, k-r), choose the group with more elements
-- 3. For remainder 0, include at most 1 element (since 0+0=0≡0 mod k)
-- 4. For remainder k/2 (when k is even), include at most 1 element
--
-- Examples:
-- >>> nonDivisibleSubset 3 [1,7,2,4]
-- 3
-- (Elements [1,7,4] have remainders [1,1,1], no pair sums to multiple of 3)
--
-- >>> nonDivisibleSubset 5 [2,7,1,4,3,6]
-- 4
nonDivisibleSubset :: Int -> [Int] -> Int
nonDivisibleSubset divisor values =
  let RemainderState hasZero hasHalf counts = buildRemainderState divisor values
      maxFromPairs = maxNonDivisibleCount divisor counts
      -- Include at most one element with remainder 0 or k/2
      zeroContribution = if hasZero then 1 else 0
      halfContribution = if hasHalf then 1 else 0
  in maxFromPairs + zeroContribution + halfContribution
