module GridSearch
(gridSearch)
where

import Data.List

data Kpm a = Kpm a [Int] deriving Show
getPattern :: Kpm a -> a
getPattern (Kpm p _) = p
getPrefixTable :: Kpm a -> [Int]
getPrefixTable (Kpm _ t) = t

prefixTable :: Eq a => [a] -> Kpm [a]
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
                         

allIndicesOf :: Eq a => Kpm [a] -> [a] -> [Int]
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

                                    

matchesAt :: [Kpm String] -> [String] -> Int -> Int -> Bool
matchesAt [] _ _ _ = True
matchesAt (firstPattern:restPatterns) grid startAt currentColumn = case Data.List.splitAt currentColumn (grid !! startAt) of
                                                                       (_,rest) -> if (isPrefixOf (getPattern firstPattern) rest) then 
                                                                                      matchesAt restPatterns grid (startAt + 1)  currentColumn
                                                                                    else False
allMatchAtAny :: [Kpm String] -> [String] -> Int -> [Int] -> Maybe Int
allMatchAtAny _ _ _ [] = Nothing
allMatchAtAny patterns grid startAt (currentColumn:restColumns) = if matchesAt patterns grid startAt currentColumn then
                                                                         Just currentColumn
                                                                      else allMatchAtAny patterns grid startAt restColumns                                                                                  
                                                                                    

gridSearch :: [String] -> [String] -> String
gridSearch grid pattern = 
                          let prefixT = getPrefixTable $ prefixTable pattern
                              kpms = Data.List.map prefixTable pattern
                              findMatch startOfMatch currentIndex  =
                                    let doOnFail = if currentIndex > 0 then
                                                      findMatch (startOfMatch + currentIndex - (prefixT !! (currentIndex - 1))) (prefixT !! (currentIndex - 1)) 
                                                   else 
                                                      findMatch (startOfMatch + 1) 0 
                                    in
                                      if currentIndex == length pattern then
                                         Just startOfMatch 
                                      else if (startOfMatch + currentIndex) >= length grid then
                                          Nothing
                                      else case  (allIndicesOf (kpms !! currentIndex)  (grid !! (startOfMatch + currentIndex))) of
                                                [] -> doOnFail
                                                allMatches -> 
                                                    case allMatchAtAny kpms grid (startOfMatch + currentIndex) allMatches  of
                                                      Nothing ->  doOnFail
                                                      something -> something
                                                        
  
                          in case findMatch 0 0 of
                              Nothing -> "NO"
                              Just _ -> "YES"
