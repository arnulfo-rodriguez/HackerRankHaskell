module GridSearch
(gridSearch)
where

import Data.List

data Kpm a = Kpm a [Int] 
getPattern (Kpm p _) = p
getPrefixTable (Kpm _ t) = t

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
 
                                    
orderedIntersect [] l2 = []
orderedIntersect l1 [] = []
orderedIntersect l1@(h1:t1) l2@(h2:t2) = if (h1 < h2) then orderedIntersect t1 l2 else if (h2 < h1) then orderedIntersect l1 t2 else h1:(orderedIntersect t1 t2)

gridSearch :: [String] -> [String] -> String
gridSearch grid pattern = let prefixT = getPrefixTable $ prefixTable pattern
                              kpms = Data.List.map prefixTable pattern
                              findMatch startOfMatch currentIndex remainingIndices =
                                    let doOnFail = if currentIndex > 0 then
                                                      findMatch (startOfMatch + currentIndex - (prefixT !! (currentIndex - 1))) (prefixT !! (currentIndex - 1))  remainingIndices
                                                   else 
                                                      findMatch (startOfMatch + 1) 0 remainingIndices
                                    in
                                      if currentIndex == length pattern then
                                         Just startOfMatch 
                                      else if (startOfMatch + currentIndex) >= length grid then
                                          Nothing
                                      else let  allMatches = (allIndicesOf (kpms !! currentIndex)  (grid !! (startOfMatch + currentIndex)))
                                            in case orderedIntersect remainingIndices allMatches of 
                                                [] -> doOnFail
                                                newRemaining -> 
                                                    case findMatch startOfMatch (currentIndex + 1) newRemaining of
                                                      Nothing -> doOnFail
                                                      something -> something
  
                          in case findMatch 0 0 [0..(length(grid !! 0) - 1)] of
                              Nothing -> "NO"
                              Just _ -> "YES"
