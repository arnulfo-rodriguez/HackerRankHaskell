{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  (
  ) where
    
import qualified Data.Set as Set
import qualified Data.List
import qualified Data.Function
import qualified Data.Ord
import qualified Data.Map.Strict



type KMods = Data.Map.Strict.Map Integer (Data.Map.Strict.Map Integer (Set.Set Integer))

getFirstLevelKey k v = let x = v `mod` k in if x > (k `div` 2) then (k - x) else x
getSecondLeveKey k v = v `mod` k

mergeMaps existingMap newMap = Data.Map.Strict.unionWith (\ s1 s2 -> Set.union s1 s2) existingMap newMap

addKmod :: Integer-> KMods -> Integer -> KMods
addKmod k kmods v  =
  let firstLevelKey = (getFirstLevelKey k v)
  in Data.Map.Strict.insertWith mergeMaps firstLevelKey (Data.Map.Strict.singleton (getSecondLeveKey k v) (Set.singleton v))  kmods

data AllSetsResult = AllSetsResult (Set.Set Integer) Integer



getAllSetsRec :: Int -> Int -> KMods -> [AllSetsResult]
getAllSetsRec n k  kmods 
  | not (Data.Map.Strict.member n kmods) = getAllSetsRec (n  + 1) k kmods
  | n == 0 = let nextSets = getAllSetsRec 1 k kmods in map (\ (AllSetsResult allLessThanK l) -> (AllSetsResult allLessThanK (l + 1))) nextSets
  | (even k) && (n == (k `div` 2))  = 
    let 
        (Just theMap) = Data.Map.Strict.lookup n kmods
        [(_ , theSet)] = Data.Map.Strict.toList theMap
        theNewSet = (Set.delete n theSet) 
    in 
        if (Set.size theNewSet) == 0 
        then [(AllSetsResult (Set.singleton n)  1)]
        else let 
              biggerThankCase = (AllSetsResult (Set.empty) 1) 
              in if (Set.size theNewSet) == (Set.size theSet) 
                  then [biggerThankCase] 
                  else [biggerThankCase,(AllSetsResult (Set.singleton n) 1)]
  | (n == (k `div` 2))  = 
    let 
        (Just theMap) = Data.Map.Strict.lookup n kmods
        theSets = Data.Map.Strict.toList theMap
        processSet (theMod, theSet) = 
          let theNewSet = (Set.delete theMod theSet) 
                in 
                  if (Set.size theNewSet) == 0 
                  then [(AllSetsResult (Set.singleton theMod)  1)]
                  else let 
                        biggerThankCase = (AllSetsResult (Set.empty) (Set.size theNewSet)) 
                        in if (Set.size theNewSet) == (Set.size theSet) 
                            then [biggerThankCase] 
                            else [biggerThankCase,(AllSetsResult (Set.singleton theMod) (Set.size theSet))]
    in concatMap processSet theSets

  | True =
    let nextSets = getAllSetsRec (n  + 1) k kmods 
        (Just theMap) = Data.Map.Strict.lookup n kmods
        theSets = Data.Map.Strict.toList theMap
        canAdd allLessThanK theMod = Data.List.all (\ ltk -> (theMod + ltk) > k) allLessThanK
        processSet prev@(AllSetsResult allLessThanK oldLength) (theMod, theSet)  = 
           let theNewSet = (Set.delete theMod theSet) 
                 in 
                   if (Set.size theNewSet) == 0 
                   then 
                     if canAdd allLessThanK theMod then [(AllSetsResult (Set.insert theMod allLessThanK)  (oldLength + 1)),prev] else [prev] 
                   else let 
                         biggerThankCase = (AllSetsResult allLessThanK  (oldLength + (Set.size theSet))) 
                         in if (Set.size theNewSet) == (Set.size theSet) 
                             then [biggerThankCase] 
                             else if canAdd allLessThanK theMode then [biggerThankCase,(AllSetsResult (Set.insert theMod allLessThanK) (oldLenth + (Set.size theSet)))] else [biggerThankCase]
     in  concatMap (\prevResult -> (concatMap (processSet prevResult) theSets)) nextSets

nonDivisibleSubset k s = let kmods =  Data.List.foldl (addKmod k) Data.Map.Strict.empty s
                         in  getAllSetsRec 0 k kmods

