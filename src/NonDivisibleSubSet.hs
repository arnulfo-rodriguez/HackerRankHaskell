{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  (nonDivisibleSubset
  ) where
    
import qualified Data.Set as Set
import qualified Data.List
import qualified Data.Function
import qualified Data.Ord
import qualified Data.Map.Strict

type KMods = Data.Map.Strict.Map Int (Data.Map.Strict.Map Int (Set.Set Int))

getFirstLevelKey k v = let x = v `mod` k in if x > (k `div` 2) then (k - x) else x
getSecondLeveKey k v = v `mod` k

mergeMaps existingMap newMap = Data.Map.Strict.unionWith (\ s1 s2 -> Set.union s1 s2) existingMap newMap

addKmod :: Int-> KMods -> Int -> KMods
addKmod k kmods v  =
  let firstLevelKey = (getFirstLevelKey k v)
  in Data.Map.Strict.insertWith mergeMaps firstLevelKey (Data.Map.Strict.singleton (getSecondLeveKey k v) (Set.singleton v))  kmods

data AllSetsResult = AllSetsResult (Set.Set Int) Int deriving(Show,Eq)
data MaxAllSets = MaxAllSets Int [AllSetsResult] | EmptyMax deriving(Show,Eq)

increaseMax EmptyMax = MaxAllSets 1 []
increaseMax (MaxAllSets m allSets) = MaxAllSets (m + 1) allSets

getAllSets EmptyMax = []
getAllSets (MaxAllSets _ allSets) = allSets

getMax EmptyMax = 0
getMax (MaxAllSets max _) = max


addSet allSet@(AllSetsResult theSet  size) EmptyMax = MaxAllSets size [allSet]
addSet allSet@(AllSetsResult theSet  size) (MaxAllSets m allSets) = MaxAllSets (max m size) (allSet:allSets)

addSets sets theMax = Data.List.foldl (flip addSet) theMax sets

getAllSetsRec :: Int -> Int -> KMods -> MaxAllSets
getAllSetsRec n k  kmods 
  | not (Data.Map.Strict.member n kmods) = getAllSetsRec (n  + 1) k kmods
  | n == 0 = let nextSets = getAllSetsRec 1 k kmods in increaseMax nextSets
  | (even k) && (n == (k `div` 2))  = 
    let 
        (Just theMap) = Data.Map.Strict.lookup n kmods
        [(_ , theSet)] = Data.Map.Strict.toList theMap
        theNewSet = (Set.delete n theSet) 
    in 
        if (Set.size theNewSet) == 0 
        then MaxAllSets 1 [(AllSetsResult (Set.singleton n)  1)]
        else let 
              biggerThankCase = MaxAllSets 1 [(AllSetsResult (Set.empty) 1)]
              in if (Set.size theNewSet) == (Set.size theSet) 
                  then biggerThankCase
                  else addSet (AllSetsResult (Set.singleton n) 1) biggerThankCase
  | (n == (k `div` 2))  = 
    let 
        (Just theMap) = Data.Map.Strict.lookup n kmods
        theSets = Data.Map.Strict.toList theMap
        processSet (theMod, theSet) =
          let theNewSet = (Set.delete theMod theSet)
                in
                  if (Set.size theNewSet) == 0
                  then  [(AllSetsResult (Set.singleton theMod)  1)]
                  else let
                        biggerThankCase = [(AllSetsResult (Set.empty) (Set.size theNewSet))]
                        in if (Set.size theNewSet) == (Set.size theSet)
                            then biggerThankCase
                            else (AllSetsResult (Set.singleton theMod)  (Set.size theSet)):biggerThankCase
    in addSets (concatMap processSet theSets) EmptyMax
  | True =
    let nextSets = getAllSetsRec (n  + 1) k kmods
        (Just theMap) = Data.Map.Strict.lookup n kmods
        theSets = Data.Map.Strict.toList theMap
        canAdd  theMod = Data.List.all (\ ltk -> (theMod + ltk) > k)
        processSet prev@(AllSetsResult allLessThanK oldLength) (theMod, theSet)  =
           let theNewSet = (Set.delete theMod theSet)
                 in
                   if (Set.size theNewSet) == 0
                   then
                     if canAdd theMod allLessThanK  then [(AllSetsResult (Set.insert theMod allLessThanK)  (oldLength + 1)),prev]  else [prev]
                   else let
                         biggerThankCase = (AllSetsResult allLessThanK  (oldLength + (Set.size theSet)))
                         canAddLtk =  canAdd theMod allLessThanK
                         containsLtk = (Set.size theNewSet) /= (Set.size theSet)
                         in if (containsLtk) && (canAddLtk)
                             then [biggerThankCase,(AllSetsResult (Set.insert theMod allLessThanK) (oldLength + (Set.size theSet)))]
                             else if containsLtk && (not canAddLtk) then [(AllSetsResult allLessThanK  (oldLength + (Set.size theNewSet)))] else [biggerThankCase]
     in  addSets (Data.List.concatMap (\ prevResult -> (Data.List.concatMap (processSet prevResult) theSets)) (getAllSets nextSets)) (MaxAllSets (getMax nextSets) [])


nonDivisibleSubset k s
  | k == 1 = 1
  | True = let kmods =  Data.List.foldl (addKmod k) Data.Map.Strict.empty s
           in  (getMax (getAllSetsRec 0 k kmods))

