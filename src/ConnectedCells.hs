module ConnectedCells (connectedCell) where
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Applicative

newtype Region = Region (Map.Map Int (Set.Set Int))

newRegion :: Int -> Int -> Region
newRegion x y = Region (Map.singleton x (Set.singleton y))

isAdjacentInUpperRow :: Int -> Int -> Region -> Maybe Bool
isAdjacentInUpperRow x y (Region theMap) = (\theSet -> Set.member (y - 1) theSet || Set.member y theSet || Set.member (y + 1) theSet) <$> Map.lookup (x - 1) theMap

isAdjacentInSameRow :: Int -> Int -> Region -> Maybe Bool
isAdjacentInSameRow x y (Region theMap) =  Set.member (y - 1) <$> Map.lookup x theMap

isAdjacentInLowerRow :: Int -> Int -> Region -> Maybe Bool
isAdjacentInLowerRow x y (Region theMap) = (\ theSet -> Set.member (y - 1) theSet || Set.member y theSet || Set.member (y + 1) theSet) <$> Map.lookup (x + 1) theMap

isPartOfRegion :: Int -> Int -> Region -> Bool
isPartOfRegion x y theRegion = Just True == (isAdjacentInUpperRow x y theRegion <|> isAdjacentInSameRow x y theRegion <|> isAdjacentInLowerRow x y theRegion)

addToRegion :: Int -> Int -> Region -> Region
addToRegion x y (Region theMap) = case (\ theSet -> Region (Map.insert x (Set.insert y theSet) theMap)) <$> Map.lookup x theMap of
                                     Nothing -> Region (Map.insert x (Set.singleton y) theMap)
                                     Just region -> region

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