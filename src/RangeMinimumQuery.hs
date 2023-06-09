{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module RangeMinimumQuery
 (theMain) where

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
import Control.Applicative


data SegmentTree = SingleSegmentNode Int Int | SegmentNode (Int, Int) Int SegmentTree SegmentTree deriving Show

getNodeMin (SingleSegmentNode _ theMin) = theMin
getNodeMin (SegmentNode _ theMin _ _) = theMin

getSegment (SingleSegmentNode idx _) = (idx, idx)
getSegment (SegmentNode theSeg _ _ _) = theSeg

myIntersection (s1,e1) (s2,e2)
 | e1 < s2 || e2 < s1 = Nothing
 | otherwise = Just (max s1 s2, min e1 e2)

buildTree :: Int -> Int -> Array Int Int -> SegmentTree
buildTree start end arr
 | start == end = SingleSegmentNode start (arr ! start)
 | otherwise =
  let half = (start + end) `div` 2
      leftTree = buildTree start half arr
      rightTree = buildTree (half + 1) end arr
  in SegmentNode (start, end)  (min (getNodeMin leftTree) (getNodeMin rightTree)) leftTree rightTree

myFindMin singleSegmentNode@(SingleSegmentNode _ theMin) seg = fmap (const theMin) $ seg `myIntersection` getSegment singleSegmentNode
myFindMin (SegmentNode theRange theMin left right) seg
  | seg == theRange = Just theMin
  | otherwise =
    let
      leftIntersection = myIntersection (getSegment left) seg
      rightIntersection = myIntersection (getSegment right) seg
      leftMin =  leftIntersection >>= myFindMin left
      rightMin = rightIntersection >>= myFindMin right
    in liftA2 min leftMin rightMin <|> leftMin <|> rightMin

theMain :: IO()
theMain = do
    firstLineStr <- System.IO.getLine
    let (arraySize, queriesSize) = case Prelude.words firstLineStr of
                                      [f,s] -> (read f :: Int,read s :: Int)
    theArrayStr <- System.IO.getLine
    let theArray = Array.listArray (0,arraySize -1) $ List.map (\ x -> read x :: Int)  $ Prelude.words theArrayStr
    let segmentTree = buildTree 0 (arraySize - 1) theArray
    forM_ [1..queriesSize] $ \_ ->
      do
        line <- System.IO.getLine
        let seg = case Prelude.words line of
                                          [f,s] -> (read f :: Int,read s :: Int)
        let result = myFindMin segmentTree seg
        print (Maybe.fromJust result)

