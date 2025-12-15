{-|
Module      : InsertionSort
Description : Solution for counting inversions in an array (Insertion Sort swaps)

== Problem Statement

Given an array of integers, determine how many swaps would be needed to sort 
the array using insertion sort. This is equivalent to counting the number of 
inversions in the array.

An inversion is a pair of indices (i, j) where i < j but array[i] > array[j].

Examples:
- [1, 2, 3, 4] → 0 swaps (already sorted)
- [4, 3, 2, 1] → 6 swaps (reverse sorted: inversions are (0,1), (0,2), (0,3), (1,2), (1,3), (2,3))
- [2, 1, 3, 1, 2] → 4 swaps

== Algorithm Overview

Despite the name "InsertionSort", this module actually implements a MERGE SORT 
algorithm to count inversions efficiently. Here's why:

**Why not actual insertion sort?**
- Real insertion sort: O(n²) time to count swaps
- This merge sort approach: O(n log n) time

**Key Insight:**
During merge sort, when merging two sorted subarrays:
- If we take an element from the RIGHT subarray before the LEFT is empty
- That element is smaller than all remaining elements in LEFT
- This creates inversions equal to the number of remaining LEFT elements

**Algorithm Steps:**
1. Divide array into two halves recursively (like standard merge sort)
2. Recursively count inversions in left and right halves
3. Count inversions during merge:
   - When taking from right subarray, add count = remaining left elements
4. Return total: left_inversions + right_inversions + merge_inversions

== Example Walkthrough

Array: [3, 1, 2]

1. Split: [3] and [1, 2]
2. Left half [3]: 0 inversions
3. Split right: [1] and [2]
4. Both halves [1], [2]: 0 inversions each
5. Merge [1], [2]: 
   - Take 1 (left=0), take 2 (left=0)
   - Merge inversions: 0
6. Merge [3] and [1, 2]:
   - Take 1 from right: inversions += 1 (from [3])
   - Take 2 from right: inversions += 1 (from [3])
   - Take 3 from left: inversions += 0
   - Merge inversions: 2
7. Total: 0 + 0 + 0 + 2 = 2 inversions

Verification: (0,1): 3>1, (0,2): 3>2 ✓

== Data Structures

Uses Array-backed Segment to avoid copying during recursion:
- Segment represents a slice [leftBound..rightBound] of the array
- Split operations create new segments without copying array data
- Only convert to Sequence for the final merge step

== Time Complexity

O(n log n) - Same as merge sort
- log n recursive levels
- O(n) work per level (merging)

Space: O(n) for temporary sequences during merging

-}
module InsertionSort(
 insertionSort
) where

import Data.Sequence as Seq
import Data.List as List
import Data.Traversable as Traversable
import Data.Foldable as Foldable
import Data.Array as Array

-- | Represents a contiguous segment of an array without copying
-- Segment leftBound rightBound array
-- This allows efficient recursive splitting by just adjusting bounds
data Segment = Segment Int Int (Array Int Int) deriving (Show)

-- | Checks if a segment is empty (leftBound > rightBound)
isEmptySegment (Segment leftBounds rightBounds _) = leftBounds > rightBounds

-- | Calculates the length of a segment
segmentLength (Segment leftBounds rightBounds _) = (rightBounds - leftBounds) + 1

-- | Splits a segment at a relative index position
-- Returns (left_segment, right_segment)
-- The index is relative to the segment's leftBound
mySplitAt ix (Segment leftBounds rightBounds array) =
  let theRealIX = leftBounds + ix
  in (Segment leftBounds (theRealIX -1) array, Segment theRealIX rightBounds array)

-- | Converts a segment to a Sequence for merging
-- Extracts elements from array[leftBound..rightBound]
toSequence (Segment leftBounds rightBounds array) = Seq.fromList $ List.map (array Array.!) [leftBounds.. rightBounds]

-- | Merges two sorted sequences while counting inversions
-- Returns (inversion_count, merged_sequence)
-- Key insight: When taking from right before left is empty,
-- we have |left| inversions (all remaining left elements are greater)
merge :: Seq Int -> Seq Int -> (Int, Seq Int)
merge Seq.Empty b = (0, b)  -- Left empty: no inversions
merge a Seq.Empty = (0, a)  -- Right empty: no inversions
merge left@(leftHead :<| leftTail) right@(rightHead :<| rightTail)
 | leftHead <= rightHead =
   -- Take from left: no new inversions (left element is in correct relative position)
   let
    (count,merged)  = merge leftTail right
   in (count, leftHead <| merged)
 | otherwise =
   -- Take from right: ALL remaining left elements are greater than rightHead
   -- This creates |left| inversions
   let
    (count, merged) = merge left rightTail
   in (count + Seq.length left, rightHead <| merged)

-- | Performs merge sort on a segment and counts inversions
-- Returns (inversion_count, sorted_sequence)
-- Base cases:
--   - Empty segment: 0 inversions
--   - Single element: 0 inversions (trivially sorted)
-- Recursive case:
--   - Split in half
--   - Recursively sort and count inversions in each half
--   - Merge and count cross-inversions
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

-- | Main function: Counts the number of swaps needed to sort the array
-- This equals the number of inversions in the array
-- 
-- Parameters:
--   - len: Length of the list
--   - lst: The list to analyze
--
-- Returns: Number of inversions (swaps needed for insertion sort)
--
-- Implementation note:
-- Despite the module name, this uses merge sort (O(n log n)) instead of
-- actual insertion sort (O(n²)) for efficiency
insertionSort len lst =
  let
    bounds@(lower,upper) = (0,len -1)
    arr = Array.listArray bounds lst
    (count, _) = segmentMergeSort (Segment lower upper arr)
  in count