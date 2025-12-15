{-|
Module      : GridSearch
Description : Solution for the 2D Grid Search problem from HackerRank

== Problem Statement

Given a large 2D grid (text matrix) and a smaller 2D pattern, determine if the 
pattern appears as a contiguous subgrid within the larger grid.

Input:
- Large grid: R rows of strings
- Small pattern: P rows of strings

Output:
- "YES" if the pattern exists as a contiguous rectangle in the grid
- "NO" otherwise

Example:
Grid:
```
7283455864
6731158619
8988242643
3830589324
2229505813
5633845374
6473530293
7053106601
0834282956
4607924137
```

Pattern:
```
9505813
3845374
530293
```

Answer: YES (pattern appears starting at row 4, column 1)

== Algorithm Overview

This solution uses a 2D variant of the Knuth-Morris-Pratt (KMP) string matching 
algorithm for efficient pattern matching:

1. **Preprocessing**: Build KMP prefix tables for each row of the pattern
   - Prefix table allows fast recovery from mismatches
   - O(m) preprocessing time per pattern row where m = pattern width

2. **2D KMP Search**: 
   - For each potential starting row in the grid:
     - Use KMP to find all column positions where the first pattern row matches
     - For each candidate column, verify all subsequent pattern rows match
     - Use KMP failure function to skip ahead on mismatches

3. **Optimization**: 
   - Build prefix table for the pattern rows themselves (treating rows as symbols)
   - On row mismatch, skip ahead using this prefix table instead of restarting
   - This reduces redundant comparisons

== KMP Algorithm Explanation

The Knuth-Morris-Pratt algorithm uses a prefix table to avoid re-comparing 
characters we've already matched. For pattern "ABABC":
- Prefix table: [0, 0, 1, 2, 0]
- If we match "ABAB" then fail on 'C', we know "AB" is both prefix and suffix
- Skip to position 2 instead of restarting from 0

== Time Complexity

O(R × C + P × Q) where:
- R × C = dimensions of the grid
- P × Q = dimensions of the pattern
- Linear time matching thanks to KMP preprocessing

Without KMP: O(R × C × P × Q) - quadratic in worst case

-}
module GridSearch
(gridSearch)
where

import Data.List

-- | Represents a pattern with its KMP prefix table
-- Stores both the pattern and its computed prefix/failure function
data Kpm a = Kpm a [Int] deriving Show

-- | Extracts the pattern from a Kpm structure
getPattern :: Kpm a -> a
getPattern (Kpm p _) = p

-- | Extracts the prefix table (failure function) from a Kpm structure
getPrefixTable :: Kpm a -> [Int]
getPrefixTable (Kpm _ t) = t

-- | Builds a KMP prefix table for a pattern
-- The prefix table at position i contains the length of the longest proper
-- prefix of pattern[0..i] that is also a suffix of pattern[0..i]
-- This is used to determine how far to backtrack on a mismatch
-- Example: pattern "ABABC" → prefix table [0, 0, 1, 2, 0]
prefixTable :: Eq a => [a] -> Kpm [a]
prefixTable pattern =  let prefixTableRec j i  currentTable = if (i == length pattern) then
                                                               currentTable
                                                            else if pattern !! i == pattern !! j then
                                                                prefixTableRec (j + 1) (i + 1) (currentTable ++ [j + 1])
                                                            else if j == 0 then
                                                                prefixTableRec j (i + 1) (currentTable ++ [0])
                                                            else
                                                                prefixTableRec (currentTable !! (j - 1)) i currentTable
                       in case pattern of
                         [_] -> Kpm pattern [0]
                         _ -> Kpm pattern $ prefixTableRec 0 1 [0]

-- | Finds all starting indices where a pattern occurs in a string using KMP
-- Returns a list of all positions where the pattern matches
-- Uses the KMP algorithm for efficient O(n + m) time complexity
-- where n = length of string, m = length of pattern
allIndicesOf :: Eq a => Kpm [a] -> [a] -> [Int]
allIndicesOf (Kpm pattern prefixT) str = let findMatch startOfMatch currentIndex =
                                                            if currentIndex == length pattern then
                                                               Just startOfMatch 
                                                            else if (startOfMatch + currentIndex) >= length str then
                                                                Nothing
                                                            else if pattern !! currentIndex == str !! (startOfMatch + currentIndex) then
                                                                findMatch startOfMatch (currentIndex + 1)
                                                            else if currentIndex > 0 then
                                                                findMatch (startOfMatch + currentIndex - (prefixT !! (currentIndex - 1))) (prefixT !! (currentIndex - 1)) 
                                                            else 
                                                                findMatch (startOfMatch + 1) 0
                                             allIndicesOfRec startOfMatch currentIndex = case findMatch startOfMatch currentIndex of
                                                                                                Nothing -> []
                                                                                                Just x -> 
                                                                                                  x : (allIndicesOfRec (x + (length pattern) - (last prefixT)) (last prefixT))
                                         in allIndicesOfRec 0 0 

-- | Checks if all pattern rows match in the grid starting at a specific position
-- Parameters:
--   - List of KPM patterns (one per pattern row)
--   - Grid (2D array of strings)
--   - Starting row in grid
--   - Column offset in grid
-- Returns True if all pattern rows match consecutively from the starting position
matchesAt :: [Kpm String] -> [String] -> Int -> Int -> Bool
matchesAt [] _ _ _ = True
matchesAt (firstPattern:restPatterns) grid startAt currentColumn = case Data.List.splitAt currentColumn (grid !! startAt) of
                                                                       (_,rest) -> if (isPrefixOf (getPattern firstPattern) rest) then 
                                                                                      matchesAt restPatterns grid (startAt + 1)  currentColumn
                                                                                    else False

-- | Tries to match the pattern at any of the given column positions
-- Parameters:
--   - List of KPM patterns (one per pattern row)
--   - Grid
--   - Starting row
--   - List of candidate column positions to try
-- Returns Just column if match found, Nothing otherwise
-- This checks multiple potential match locations in a single row
allMatchAtAny :: [Kpm String] -> [String] -> Int -> [Int] -> Maybe Int
allMatchAtAny _ _ _ [] = Nothing
allMatchAtAny patterns grid startAt (currentColumn:restColumns) = if matchesAt patterns grid startAt currentColumn then
                                                                         Just currentColumn
                                                                      else allMatchAtAny patterns grid startAt restColumns

-- | Main function: Searches for a 2D pattern within a 2D grid
-- Uses a 2D variant of the KMP algorithm:
-- 1. Builds KMP prefix tables for each pattern row
-- 2. Builds a prefix table for the pattern rows themselves
-- 3. Searches row by row, using KMP to find potential column matches
-- 4. For each candidate, verifies all pattern rows match
-- 5. On failure, uses prefix table to skip ahead efficiently
--
-- Parameters:
--   - grid: The large 2D text grid to search in
--   - pattern: The smaller 2D pattern to find
--
-- Returns:
--   - "YES" if pattern found as contiguous subgrid
--   - "NO" otherwise
gridSearch :: [String] -> [String] -> String
gridSearch grid pattern = 
                          let prefixT = getPrefixTable $ prefixTable pattern
                              kpms = Data.List.map prefixTable pattern
                              findMatch startOfMatch currentIndex  =
                                    let doOnFail = if currentIndex > 0 then
                                                      findMatch (startOfMatch + currentIndex - (prefixT !! (currentIndex - 1))) (prefixT !! (currentIndex - 1)) 
                                                   else 
                                                      findMatch (startOfMatch + 1) 0 
                                    in
                                      if currentIndex == length pattern then
                                         Just startOfMatch 
                                      else if (startOfMatch + currentIndex) >= length grid then
                                          Nothing
                                      else case  (allIndicesOf (kpms !! currentIndex)  (grid !! (startOfMatch + currentIndex))) of
                                                [] -> doOnFail
                                                allMatches -> 
                                                    case allMatchAtAny kpms grid (startOfMatch + currentIndex) allMatches  of
                                                      Nothing ->  doOnFail
                                                      something -> something
                                                        
  
                          in case findMatch 0 0 of
                              Nothing -> "NO"
                              Just _ -> "YES"
