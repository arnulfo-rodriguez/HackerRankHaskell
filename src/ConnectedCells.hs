module ConnectedCells (connectedCell) where
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Set as Set
import qualified Data.List as List

newtype Region = Region (Map.Map Int (Set.Set Int))

newRegion :: Int -> Int -> Region
newRegion x y = Region (Map.singleton x (Set.singleton y))

isAdjacentInUpperRow :: Int -> Int -> Region -> Bool
isAdjacentInUpperRow x y (Region theMap) = case Map.lookup (x - 1) theMap of
                                              Nothing -> False
                                              Just theSet -> Set.member (y - 1) theSet || Set.member y theSet || Set.member (y + 1) theSet

isAdjacentInSameRow :: Int -> Int -> Region -> Bool
isAdjacentInSameRow x y (Region theMap) = case Map.lookup x theMap of
                                              Nothing -> False
                                              Just theSet -> Set.member (y - 1) theSet  || Set.member (y + 1) theSet

isAdjacentInLowerRow :: Int -> Int -> Region -> Bool
isAdjacentInLowerRow x y (Region theMap) = case Map.lookup (x + 1) theMap of
                                              Nothing -> False
                                              Just theSet -> Set.member (y - 1) theSet || Set.member y theSet || Set.member (y + 1) theSet


isPartOfRegion :: Int -> Int -> Region -> Bool
isPartOfRegion x y theRegion = isAdjacentInUpperRow x y theRegion || isAdjacentInSameRow x y theRegion || isAdjacentInLowerRow x y theRegion

addToRegion :: Int -> Int -> Region -> Region
addToRegion x y (Region theMap) = case Map.lookup x theMap of
                                     Nothing -> Region (Map.insert x (Set.singleton y) theMap)
                                     Just theSet -> Region (Map.insert x (Set.insert y theSet) theMap)

mergeRegions (Region theMap1) (Region theMap2) = Region (Map.Merge.merge
                                                    Map.Merge.preserveMissing
                                                    Map.Merge.preserveMissing
                                                    (Map.Merge.zipWithAMatched (\ _ v1 v2 ->  pure (Set.union v1 v2)))
                                                    theMap1
                                                    theMap2)
mergeAllRegions:: Region -> [Region] -> Region
mergeAllRegions = List.foldl mergeRegions

regionSize :: Region -> Int
regionSize (Region theMap) =  List.sum $  List.map  Set.size  $ Map.elems theMap

addToCorrespondingRegion :: Int -> Int -> [Region] -> [Region]
addToCorrespondingRegion x y otherRegions = case List.partition (isPartOfRegion x y) otherRegions of
                                              ([],_) -> newRegion x y:otherRegions
                                              (intersectingRegions,disjointRegions) -> mergeAllRegions (newRegion x y) intersectingRegions:disjointRegions

buildRegions :: (Eq a, Num a) => Int -> Int -> [[a]] -> [Region]
buildRegions _ _ [] = []
buildRegions x _ ([]:r) = buildRegions (x + 1) 0 r
buildRegions x y ((h:rowRest):rest)
 | h == 0 = buildRegions x (y + 1) (rowRest:rest)
 | h == 1 = case buildRegions x (y + 1) (rowRest:rest) of
              newRegions -> addToCorrespondingRegion x y newRegions

connectedCell :: (Eq a, Num a) => [[a]] -> Int
connectedCell matrix =  List.maximum $ List.map regionSize  $ buildRegions 0 0 matrix