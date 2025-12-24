{-|
Module      : OrganizingContainers
Description : Solution for the Organizing Containers of Balls problem from HackerRank

== Problem Statement

You have N containers, each currently holding balls of various colors. You can perform 
unlimited swap operations between any two containers (swap one ball from container i 
with one ball from container j).

Goal: Determine if it's possible to organize the balls such that each container holds 
balls of only ONE color (though different containers can hold different colors).

Input: An N×N matrix where matrix[i][j] = number of balls of color j in container i

Output: "Possible" if organization is achievable, "Impossible" otherwise

Example:
```
Container 0: [1, 1]  -- 1 ball of color 0, 1 ball of color 1
Container 1: [1, 1]  -- 1 ball of color 0, 1 ball of color 1
```
Answer: Possible
- Container 0 capacity = 2, Container 1 capacity = 2
- Color 0 total = 2, Color 1 total = 2
- We can organize: Container 0 gets all color 0, Container 1 gets all color 1

Example:
```
Container 0: [1, 3, 1]  -- total capacity = 5
Container 1: [2, 1, 2]  -- total capacity = 5
Container 2: [3, 3, 3]  -- total capacity = 9
```
Answer: Impossible
- Container capacities: [5, 5, 9]
- Color totals: [6, 7, 6] (color 0=1+2+3=6, color 1=3+1+3=7, color 2=1+2+3=6)
- Sorted: [5,5,9] ≠ [6,6,7]
- No valid assignment exists

== Algorithm: Capacity-Color Matching

The key insight is that this is a bipartite matching problem:
- Left side: Containers with their capacities
- Right side: Colors with their ball counts
- We need a perfect matching where each container is assigned exactly one color

For a container to hold all balls of a specific color:
- Container's capacity must equal the total number of balls of that color

The solution uses the **Greedy Matching Theorem**:
If we can match containers to colors by pairing them in sorted order (largest to 
largest, etc.), then a valid assignment exists.

Proof sketch:
1. Each container must end up with exactly as many balls as it currently has (capacity)
2. Each color must be completely assigned to one container
3. If sorted(capacities) = sorted(color_totals), we can greedily match:
   - Largest capacity container ← color with most balls
   - Second largest capacity ← color with second most balls
   - And so on...
4. This greedy matching is always valid by Hall's Marriage Theorem

== Implementation

1. Calculate container capacities: sum of each row
2. Calculate color totals: sum of each column (via transpose)
3. Sort both lists
4. If equal: assignment exists (Possible)
   If not equal: no valid assignment (Impossible)

== Time Complexity

- O(N²) to transpose the matrix
- O(N log N) to sort both lists
- Overall: O(N²)

== Space Complexity

O(N) for the sorted lists

-}
module OrganizingContainers where

import Data.List

-- | Determines if balls can be organized so each container has only one color
-- 
-- Parameters:
--   container: N×N matrix where container[i][j] = count of color j balls in container i
-- 
-- Returns:
--   "Possible" if balls can be reorganized into single-color containers
--   "Impossible" if no valid organization exists
-- 
-- The algorithm compares sorted container capacities with sorted color totals.
-- A valid assignment exists if and only if these sorted lists are equal.
organizingContainers:: [[Int]] -> String
organizingContainers container = 
   let itemsPerContainer = sort $ map sum container         -- Sort container capacities
       totalPerColor = sort $ map sum $ Data.List.transpose container  -- Sort color totals
   in if totalPerColor == itemsPerContainer
       then "Possible"   -- Greedy matching exists
       else "Impossible"  -- No valid assignment