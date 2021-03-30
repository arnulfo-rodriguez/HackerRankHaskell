{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  (nonDivisibleSubset
  ) where

import qualified Data.Set as Set
import qualified Data.List
import qualified Data.Function
import qualified Data.Ord
import qualified Data.Map.Strict

increaseAllThatApply :: Int -> Int -> (Data.Map.Strict.Map Int Int) -> (Data.Map.Strict.Map Int Int) 
increaseAllThatApply k newModule theMap = (Data.Map.Strict.insertWith (+) newModule 1 theMap)

addToMods :: Int -> (Bool, Bool, Data.Map.Strict.Map Int Int) -> Int -> (Bool, Bool, Data.Map.Strict.Map Int Int) 
addToMods k (hasMod0, hasModHalfK, theMap) value
 | (even k) && ((value `mod` k) == (k `div` 2)) = (hasMod0, True, theMap)
 | (value `mod` k) == 0 = (True, hasModHalfK, theMap)
 | True = (hasMod0, hasModHalfK, (increaseAllThatApply k (value `mod` k) theMap))

nonDivisibleSubsetRec k [] = (False,False,Data.Map.Strict.empty)
nonDivisibleSubsetRec k (h:rest) = let nextMods = nonDivisibleSubsetRec k rest
                                in addToMods k nextMods h
getMax :: Int -> (Data.Map.Strict.Map Int Int) -> Int                          
getMax k theMap =  let
                       getMaxRec :: Int -> Int
                       getMaxRec i =  if i > (k `div` 2) 
                                      then 0 
                                      else 
                                         let theMax = getMaxRec (i + 1)
                                         in case ((Data.Map.Strict.lookup i theMap),(Data.Map.Strict.lookup (k - i) theMap)) of
                                           (Nothing,Nothing) -> theMax
                                           (Just a, Nothing) -> theMax + a
                                           (Nothing, Just b) -> theMax + b
                                           (Just a,Just b) -> theMax + (max a b)   
                   in getMaxRec 1
                                                    
                                
nonDivisibleSubset :: Int -> [Int] -> Int
nonDivisibleSubset k s =
  let (hasMod0, hasModHalfK, subsets) = nonDivisibleSubsetRec k s
      theMax = getMax k subsets
  in  theMax + (if hasMod0 then 1 else 0)  + (if hasModHalfK then 1 else 0)
