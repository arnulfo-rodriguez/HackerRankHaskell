module SubstringSearching
(containsPat)
where

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.Set
import Data.Text as Text
import Data.Text.IO as TIO
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Array as Array
import Data.Sequence as Seq


prefixTable pattern =
  let
    prefixTableRec :: Int-> [Char] -> [Char] -> State Int [Int]
    prefixTableRec count _ [] = do
        modify (+1)
        return [count]
    prefixTableRec count (ph:pRest) (sh:sRest)
      | ph == sh = do
          modify (+1)
          recursiveCallResult <- prefixTableRec (count + 1) pRest sRest
          return (count:recursiveCallResult)
      | otherwise  = do
          modify (+ 1)
          recursiveCallResult <- prefixTableRec (-1) pattern sRest
          return (count:recursiveCallResult)
  in runState (prefixTableRec (-1) pattern (List.tail pattern)) 0

nextIndexOf:: [Char] -> Array Int Char -> Maybe Int
nextIndexOf pattern theStr =
  let
    (pTable, patLength) = prefixTable pattern
    thePrefixTable = Array.listArray (0, (patLength - 1)) $ pTable
    patternSeq = Array.listArray (0, (patLength - 1)) pattern
    theStrArray = theStr
    nextIndexOfRec startIdx patIdx
      | (patIdx + startIdx) == (Array.rangeSize (Array.bounds theStrArray)) && patIdx == patLength = Just startIdx
      | (patIdx + startIdx) == (Array.rangeSize (Array.bounds theStrArray)) = Nothing
    nextIndexOfRec startIdx patIdx
      | patIdx == patLength = Just startIdx
      | (patternSeq ! patIdx) == (theStrArray ! (startIdx + patIdx)) = nextIndexOfRec startIdx (patIdx + 1)
      | (thePrefixTable ! patIdx) >= 0 = nextIndexOfRec (startIdx + (patIdx - (thePrefixTable ! patIdx))) (thePrefixTable ! patIdx)
      | otherwise = nextIndexOfRec (startIdx + 1 ) 0
  in nextIndexOfRec 0 0

containsPat pat str =
  let indices = nextIndexOf pat str
  in maybe "NO" (\ _ -> "YES") indices