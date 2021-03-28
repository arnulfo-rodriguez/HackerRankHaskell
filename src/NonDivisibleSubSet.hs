{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  (nonDivisibleSubset
  ) where
    
import qualified Data.Set as Set
import qualified Data.List
import qualified Data.Function
import qualified Data.Ord
import qualified Data.Map.Strict

data KMod = KMod Bool Int
type KMods = Data.Map.Strict.Map Int (Data.Map.Strict.Map Int KMod)

data AllSetsResult = AllSetsResult (Set.Set Int) Int deriving(Show,Eq)
data MaxAllSets = MaxAllSets Int [AllSetsResult] | EmptyMax deriving(Show,Eq)

singleKMod k i = KMod (i < k) 1

addToKmod k i (KMod hasLessThanK count) = KMod ((i < k) || hasLessThanK) (count + 1)
mergeKmods (KMod hasLessThank1 count1) (KMod hasLessThank2 count2) = KMod (hasLessThank1 || hasLessThank2) (count1 + count2)

hasLessThanKMember (KMod hasLessThanK _) = hasLessThanK
kModLength (KMod _ l) = l

getFirstLevelKey k v = let x = v `mod` k in if x > (k `div` 2) then (k - x) else x
getSecondLeveKey k v = v `mod` k

mergeMaps existingMap newMap = Data.Map.Strict.unionWith (\ s1 s2 -> mergeKmods s1 s2) existingMap newMap

addKmod :: Int-> KMods -> Int -> KMods
addKmod k kmods v  =
  let firstLevelKey = (getFirstLevelKey k v)
      getKmod = (singleKMod k)
  in Data.Map.Strict.insertWith mergeMaps firstLevelKey (Data.Map.Strict.singleton (getSecondLeveKey k v) (getKmod v))  kmods


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
        [(_ , theKMod)] = Data.Map.Strict.toList theMap
    in 
        if (hasLessThanKMember theKMod)  && ((kModLength theKMod) == 1)
        then MaxAllSets 1 [(AllSetsResult (Set.singleton n)  1)]
        else let 
              biggerThankCase = MaxAllSets 1 [(AllSetsResult (Set.empty) 1)]
              in if not (hasLessThanKMember theKMod) 
                  then biggerThankCase
                  else addSet (AllSetsResult (Set.singleton n) 1) biggerThankCase
  | (n == (k `div` 2))  = 
    let 
        (Just theMap) = Data.Map.Strict.lookup n kmods
        theKMods = Data.Map.Strict.toList theMap
        processSet (theMod, theKMods) =
                  if  (hasLessThanKMember theKMods) && ((kModLength theKMods) == 1)
                  then  [(AllSetsResult (Set.singleton theMod)  1)]
                  else let
                        biggerThankCase = [(AllSetsResult (Set.empty) ((kModLength theKMods) - 1))]
                        in if not (hasLessThanKMember theKMods)
                            then biggerThankCase
                            else (AllSetsResult (Set.singleton theMod)  (kModLength theKMods)):biggerThankCase
    in addSets (concatMap processSet theKMods) EmptyMax
  | True =
    let nextSets = getAllSetsRec (n  + 1) k kmods
        (Just theMap) = Data.Map.Strict.lookup n kmods
        theSets = Data.Map.Strict.toList theMap
        canAdd  theMod = Data.List.all (\ ltk -> (theMod + ltk) > k)
        processSet prev@(AllSetsResult allLessThanK oldLength) (theMod, theKMods)  =
                  if  (hasLessThanKMember theKMods) && ((kModLength theKMods) == 1)
                   then
                     if canAdd theMod allLessThanK  then [(AllSetsResult (Set.insert theMod allLessThanK)  (oldLength + 1)),prev]  else [prev]
                   else let
                         biggerThankCase = (AllSetsResult allLessThanK  (oldLength + (kModLength theKMods)))
                         canAddLtk =  canAdd theMod allLessThanK
                         containsLtk = (hasLessThanKMember theKMods)
                         in if (containsLtk) && (canAddLtk)
                             then [biggerThankCase,(AllSetsResult (Set.insert theMod allLessThanK) (oldLength + (kModLength theKMods)))]
                             else if containsLtk && (not canAddLtk) then [(AllSetsResult allLessThanK  (oldLength + (kModLength theKMods)))] else [biggerThankCase]
     in  addSets (Data.List.concatMap (\ prevResult -> (Data.List.concatMap (processSet prevResult) theSets)) (getAllSets nextSets)) (MaxAllSets (getMax nextSets) [])


nonDivisibleSubset k s
--  | k == 1 = EmptyMax
  | True = let kmods =  Data.List.foldl (addKmod k) Data.Map.Strict.empty s
           in kmods
--           in (getAllSets (getAllSetsRec 0 k kmods))

