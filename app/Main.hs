{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.Set
import Data.Text
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

allIndicesOf:: [Char] -> [Char] -> [Int]
allIndicesOf pattern theStr =
  let
    (pTable, patLength) = prefixTable pattern
    thePrefixTable = Array.listArray (0, (patLength - 1)) $ pTable
    patternSeq = Array.listArray (0, (patLength - 1)) pattern
    allIndicesOfRec startIdx patIdx []
      | patIdx == patLength = [startIdx]
      | otherwise = []
    allIndicesOfRec startIdx patIdx str@(hStr:restStr)
      | patIdx == patLength = startIdx:allIndicesOfRec (startIdx + patLength) 0 str
      | (patternSeq ! patIdx) == hStr = allIndicesOfRec startIdx (patIdx + 1) restStr
      | (patIdx > 0) && (thePrefixTable ! patIdx) >= 0 = allIndicesOfRec (startIdx + (patIdx - (thePrefixTable ! patIdx))) (thePrefixTable ! patIdx) str
      | patIdx > 0 = allIndicesOfRec (startIdx + 1) 0 str
      | otherwise = allIndicesOfRec (startIdx + 1) 0 restStr
  in allIndicesOfRec 0 0 theStr

containsPat pat str =
  let indices = allIndicesOf pat str
  in if List.null indices then "NO" else "YES"

toPairs [] = []
toPairs (f:s:rest) = (f,s):toPairs rest

main :: IO()
main = do
    sizeStr <- getLine
    let size =  read  sizeStr :: Int
    words <- forM [1..(2 * size)] $ \_ ->
      do
        line <- getLine
        return line
    let pairs = toPairs words
    forM_ pairs $ \(word,pattern) ->
      do
        putStrLn (containsPat pattern word)
