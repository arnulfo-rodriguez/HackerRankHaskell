{-|
Module      : MaxSubsetSum
Description : Solution for the Maximum Sum with Non-Adjacent Elements problem

== Problem Statement

Given an array of integers, find the maximum sum of a subset where no two 
elements in the subset are adjacent in the original array.

This is also known as the "House Robber" problem or "Non-Adjacent Maximum Sum".

Examples:
- [3, 2, 7, 10] → 13 (take 3 + 10)
- [3, 2, 5, 10, 7] → 15 (take 3 + 5 + 7)
- [5, 1, 1, 5] → 10 (take 5 + 5)
- [-2, 1, 3, -4, 5] → 8 (take 1 + 3 + 5, skip negatives)

Constraint: You cannot take two adjacent elements.

== Algorithm Overview

This solution uses dynamic programming with a recursive approach that processes
the array from right to left (reverse order).

**Key Insight:**
For each element at position i, we have two choices:
1. Include element[i]: Then we must skip element[i+1], can use element[i+2]
2. Skip element[i]: Then we can consider element[i+1]

The maximum sum at position i is: max(element[i] + maxSum[i+2], maxSum[i+1])

**State Representation:**
The recursion returns a tuple (current_max, previous_max):
- current_max: Max sum from current position onwards
- previous_max: Max sum from next position onwards (Just value or Nothing)

**Base Cases:**
- Single element [a]: Return (a, Nothing) - only one choice
- No previous element: Nothing indicates we're at the end

**Recursive Case:**
For element 'a' with remaining elements having maxes (m1, m2):
- m1 = max sum starting from next element
- m2 = max sum starting from element after next
- Choice 1: Take 'a' alone = a
- Choice 2: Skip 'a', take m1's solution = m2
- Choice 3: Take 'a' + m2 (skip next) = a + m2
- Result: (max[a, m2, a+m2], Just(max(m1, m2)))

The recursion naturally enforces the non-adjacency constraint by tracking
two positions ahead.

== Example Walkthrough

Array: [3, 2, 7, 10]

Process right-to-left:
1. [10]: (10, Nothing)
2. [7, 10]: 
   - m1=10, m2=Nothing
   - max(7, 0, 7+0) = 7, but m1=10 is better
   - Result: (10, Just 10)
3. [2, 7, 10]:
   - m1=10, m2=Just 10
   - max(2, 10, 2+10) = 12
   - Result: (12, Just 10)
4. [3, 2, 7, 10]:
   - m1=12, m2=Just 10
   - max(3, 10, 3+10) = 13
   - Result: (13, Just 12)

Final: max(13, 12) = 13 ✓

== Time Complexity

O(n) - Single pass through the array (recursive calls process each element once)
Space: O(n) - Call stack depth for recursion

This could be optimized to O(1) space with iterative DP using just two variables,
but the recursive solution is elegant and clear.

-}
module MaxSubsetSum where

-- | Recursive helper function that processes array from right to left
-- Returns (max_sum_from_here, max_sum_from_next_position)
-- 
-- The tuple structure allows us to:
-- - Track the best solution from current position (first element)
-- - Remember the best from the next position for adjacency check (second element)
--
-- Base case: Single element returns (element, Nothing) since there's no next position
maxSubsetSumRec [a] = (a,Nothing)
maxSubsetSumRec (a:rest) =  case maxSubsetSumRec rest of
                             -- When only one element remains in rest:
                             -- We can take current 'a' alone, or take the next element m1
                             (m1,Nothing) -> (a, Just m1)
                             -- When we have results from two positions ahead:
                             -- m1 = max from next position (i+1)
                             -- m2 = max from position after next (i+2)
                             -- We choose: take 'a', take m2 (skip next), or take 'a'+m2
                             (m1, Just m2) -> (maximum [a,m2,a+m2], Just (max m1 m2))

-- | Main function: Computes maximum subset sum with non-adjacent constraint
-- 
-- Takes an array and returns the maximum sum achievable by selecting
-- a subset of non-adjacent elements.
--
-- Algorithm:
--   1. Calls recursive helper to compute (max_from_start, max_from_position_1)
--   2. Returns the maximum of these two values
--   3. This handles the case where optimal solution may not include first element
--
-- Example: maxSubsetSum [3, 2, 7, 10] = 13
maxSubsetSum arr = case maxSubsetSumRec arr of
                          (a,Nothing) -> a
                          (a, Just b) -> max a b