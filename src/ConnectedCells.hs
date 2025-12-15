{-|
Module      : ConnectedCells
Description : Solution for finding the largest connected region in a binary matrix

== Problem Statement

Given a binary matrix (containing only 0s and 1s), find the size of the largest 
connected region. A region consists of cells containing 1s that are connected 
horizontally, vertically, or diagonally.

Example:
```
1 1 0 0
0 1 1 0
0 0 1 0
1 0 0 0
```

Here we have two regions:
- Region 1: 6 cells (top-left cluster connected via diagonal/adjacent)
- Region 2: 1 cell (bottom-left single cell)

The answer is 6 (the largest region size).

== Algorithm Overview

The solution uses a single-pass region-building algorithm with region merging:

1. Process the matrix left-to-right, top-to-bottom
2. For each cell containing 1:
   - Check if it's adjacent to any existing regions
   - If adjacent to one region: add it to that region
   - If adjacent to multiple regions: merge those regions and add the cell
   - If not adjacent to any: create a new region
3. Return the size of the largest region

A cell at (x, y) is adjacent to a region if any of its 8 neighbors belong to that region:
- Upper row: (x-1, y-1), (x-1, y), (x-1, y+1)
- Same row: (x, y-1), (x, y+1)
- Lower row: (x+1, y-1), (x+1, y), (x+1, y+1)

== Data Structures

Region is represented as a Map from row index to Set of column indices:
- Allows efficient membership checking
- Supports efficient merging of regions
- Space-efficient representation

== Time Complexity

O(R × C × log(R × C)) where R = rows, C = columns
- Each cell is processed once
- Region operations are logarithmic due to Map/Set data structures

-}
module ConnectedCells (connectedCell) where
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Applicative

-- | Represents a connected region of cells
-- Maps row indices to sets of column indices for efficient lookup
newtype Region = Region (Map.Map Int (Set.Set Int))

-- | Creates a new region containing a single cell at position (x, y)
newRegion :: Int -> Int -> Region
newRegion x y = Region (Map.singleton x (Set.singleton y))

-- | Checks if cell (x, y) is adjacent to the region via the upper row (x-1)
-- Returns Nothing if row (x-1) doesn't exist in the region
-- Returns Just True if any of (x-1, y-1), (x-1, y), or (x-1, y+1) are in the region
isAdjacentInUpperRow :: Int -> Int -> Region -> Maybe Bool
isAdjacentInUpperRow x y (Region theMap) = (\theSet -> Set.member (y - 1) theSet || Set.member y theSet || Set.member (y + 1) theSet) <$> Map.lookup (x - 1) theMap

-- | Checks if cell (x, y) is adjacent to the region via the same row (x)
-- Returns Nothing if row (x) doesn't exist in the region
-- Returns Just True if (x, y-1) is in the region (left neighbor)
-- Note: Only checks left because we process left-to-right
isAdjacentInSameRow :: Int -> Int -> Region -> Maybe Bool
isAdjacentInSameRow x y (Region theMap) =  Set.member (y - 1) <$> Map.lookup x theMap

-- | Checks if cell (x, y) is adjacent to the region via the lower row (x+1)
-- Returns Nothing if row (x+1) doesn't exist in the region
-- Returns Just True if any of (x+1, y-1), (x+1, y), or (x+1, y+1) are in the region
-- Note: Lower row can be populated from diagonal connections in previous rows
isAdjacentInLowerRow :: Int -> Int -> Region -> Maybe Bool
isAdjacentInLowerRow x y (Region theMap) = (\ theSet -> Set.member (y - 1) theSet || Set.member y theSet || Set.member (y + 1) theSet) <$> Map.lookup (x + 1) theMap

-- | Determines if cell (x, y) should be part of the given region
-- Returns True if the cell is adjacent to the region in any direction
-- Uses Alternative (<|>) to check all three possible adjacencies
isPartOfRegion :: Int -> Int -> Region -> Bool
isPartOfRegion x y theRegion = Just True == (isAdjacentInUpperRow x y theRegion <|> isAdjacentInSameRow x y theRegion <|> isAdjacentInLowerRow x y theRegion)

-- | Adds a cell at position (x, y) to an existing region
-- If row x already exists, adds y to that row's column set
-- If row x doesn't exist, creates a new entry for row x with column y
addToRegion :: Int -> Int -> Region -> Region
addToRegion x y (Region theMap) = case (\ theSet -> Region (Map.insert x (Set.insert y theSet) theMap)) <$> Map.lookup x theMap of
                                     Nothing -> Region (Map.insert x (Set.singleton y) theMap)
                                     Just region -> region

-- | Merges two regions into a single region
-- For rows that exist in both regions, unions their column sets
-- For rows in only one region, preserves them as-is
mergeRegions (Region theMap1) (Region theMap2) = Region (Map.Merge.merge
                                                    Map.Merge.preserveMissing
                                                    Map.Merge.preserveMissing
                                                    (Map.Merge.zipWithAMatched (\ _ v1 v2 ->  pure (Set.union v1 v2)))
                                                    theMap1
                                                    theMap2)

-- | Merges a base region with a list of additional regions
-- Used when a cell connects multiple previously-disjoint regions
mergeAllRegions:: Region -> [Region] -> Region
mergeAllRegions = List.foldl mergeRegions

-- | Calculates the total number of cells in a region
-- Sums the sizes of all column sets across all rows
regionSize :: Region -> Int
regionSize (Region theMap) =  List.sum $  List.map  Set.size  $ Map.elems theMap

-- | Adds a cell to the appropriate region(s) in the list
-- Three cases:
-- 1. Cell not adjacent to any region: Create new single-cell region
-- 2. Cell adjacent to one region: Add to that region
-- 3. Cell adjacent to multiple regions: Merge those regions and add cell
-- Returns updated list of regions
addToCorrespondingRegion :: Int -> Int -> [Region] -> [Region]
addToCorrespondingRegion x y otherRegions = case List.partition (isPartOfRegion x y) otherRegions of
                                              ([],_) -> newRegion x y:otherRegions
                                              (intersectingRegions,disjointRegions) -> mergeAllRegions (newRegion x y) intersectingRegions:disjointRegions

-- | Recursively builds all regions from the matrix
-- Processes the matrix row by row, left to right
-- For each cell containing 1, adds it to the appropriate region(s)
-- For cells containing 0, skips them
-- Returns list of all connected regions
buildRegions :: (Eq a, Num a) => Int -> Int -> [[a]] -> [Region]
buildRegions _ _ [] = []
buildRegions x _ ([]:r) = buildRegions (x + 1) 0 r
buildRegions x y ((h:rowRest):rest)
 | h == 0 = buildRegions x (y + 1) (rowRest:rest)
 | h == 1 = case buildRegions x (y + 1) (rowRest:rest) of
              newRegions -> addToCorrespondingRegion x y newRegions

-- | Main function: Finds the size of the largest connected region
-- Takes a binary matrix (containing 0s and 1s)
-- Returns the maximum region size found
-- Processes by building all regions, then finding the maximum size
connectedCell :: (Eq a, Num a) => [[a]] -> Int
connectedCell matrix =  List.maximum $ List.map regionSize  $ buildRegions 0 0 matrix