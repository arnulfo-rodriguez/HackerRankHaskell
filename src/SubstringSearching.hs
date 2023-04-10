module SubstringSearching
(containsPat)
where

import Data.List
import Data.Maybe as Maybe
import Control.Monad.State
import Data.Array as Array

data Kpm a = Kpm a [Int] deriving Show
type SearchState = State Int
type IndexInStr = Int
type IndexInPattern = Int
data NextIndexOfResult = Fail IndexInPattern IndexInStr [Char] | Success IndexInStr [Char]

prefixTable pattern =
  let
    prefixTableRec count _ [] = [count]
    prefixTableRec count (ph:pRest) (sh:sRest)
      | ph == sh = count:(prefixTableRec (count + 1) pRest sRest)
      | otherwise = count:(prefixTableRec 0 pattern sRest)
  in (prefixTableRec 0 pattern (tail pattern))

allIndicesOf:: [Char] -> [Char] -> [Int]
allIndicesOf pattern theStr =
  let
    patLength = Data.List.length pattern
    thePrefixTable = Array.listArray (0, (patLength - 1)) $ prefixTable pattern
    patternSeq = Array.listArray (0, (patLength - 1)) pattern
    allIndicesOfRec startIdx patIdx []
      | patIdx == patLength = [startIdx]
      | otherwise = [] 
    allIndicesOfRec startIdx patIdx str@(hStr:restStr)
      | patIdx >= patLength = startIdx:allIndicesOfRec (startIdx + patLength) 0 str
      | (patternSeq ! patIdx) == hStr = allIndicesOfRec startIdx (patIdx + 1) restStr
      | patIdx > 0 = allIndicesOfRec (startIdx + (patIdx - (thePrefixTable ! patIdx))) (thePrefixTable ! patIdx) str
      | otherwise = allIndicesOfRec (startIdx + 1) 0 restStr
  in allIndicesOfRec 0 0 theStr

containsPat pat str =
  let indices = allIndicesOf pat str
  in if Data.List.null indices then "FALSE" else "TRUE"