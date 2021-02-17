{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  ( nonDivisibleSubset
  ) where
    
import Data.List
import Data.Function
  
scalaGroupBy :: Ord b => (a -> b) -> [a] -> [[a]]
scalaGroupBy f = Data.List.groupBy ((==) `Data.Function.on` f) . Data.List.sortOn f

mergeSorted :: Foldable t => [t a] -> [t a] -> [t a]
mergeSorted [] [] = []
mergeSorted a []  = a
mergeSorted [] b = b
mergeSorted l1@(h1:rest1) l2@(h2:rest2) = if Data.List.length h1 > Data.List.length h2 then h1:mergeSorted rest1 l2 else h2:mergeSorted l1 rest2
  
combine :: Num a => (a -> Bool) -> [a] -> [[a]]
combine _  []  = []
combine p (h:rest) =  let combined = combine p rest 
                          newSets = Data.List.map (h :) (Data.List.filter (Data.List.all (\ e -> p (h + e))) combined)  
                       in
                          mergeSorted combined newSets ++ [[h]]
 
   
nonDivisibleSubset :: Integral a => a -> [a] -> Int
nonDivisibleSubset k s =
  let 
    f =   (\ x -> x `mod` k /= 0)
    splittedByMod = scalaGroupBy f s
    getMaxArray = (Data.List.length . Data.List.head . combine f)
  in case splittedByMod of
      [all@(h:_)] -> if f h then getMaxArray all else 1
      [[],nonDivByK] ->  getMaxArray nonDivByK 
      [_,[]] -> 1
      [_,nonDivByK] -> 1 + getMaxArray nonDivByK
