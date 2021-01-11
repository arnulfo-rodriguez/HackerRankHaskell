module MinimumLoss
(minimumLoss) where

import Data.Maybe


insrt :: (Ord a, Num a) => a -> [a] -> (Maybe a, [a])
insrt item [] =  (Nothing , [item])
insrt item [i] = if  item > i then (Just (item - i) , [i,item]) else  (Nothing , [item,i]) 
insrt item items = let (start,h:tl) = splitAt (length items `div` 2) items
                           in if  item > h then
                               let (minLoss,newTail) = insrt item (h:tl)
                               in (minLoss, start ++ newTail)
                           else
                               let (minLoss,newStart) = insrt item start
                               in (minLoss, newStart ++ (h:tl))
  

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just $ min a b

accumulate :: (Ord a, Num a) => a -> (Maybe a, [a])  -> (Maybe a, [a])
accumulate i (currentMin,acc) = let (m,newLst) = insrt i  acc in (minMaybe currentMin m, newLst)

minimumLossOpt :: (Foldable t, Ord a, Num a) => t a -> Maybe a
minimumLossOpt lst = let (result,_) = foldr accumulate (Nothing, []) lst
                      in result

minimumLoss :: (Num a, Ord a) => [a] -> a
minimumLoss price = Data.Maybe.fromMaybe (- 1) (minimumLossOpt price)
