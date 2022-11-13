module SwapNodes
(swapNTimes) where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

type NodeValue = Int
data BinaryTree = EmptyBinaryTree | BinaryTree NodeValue BinaryTree BinaryTree | Leaf NodeValue deriving (Show)
data TreeBuilder = EBuilder | LBuilder NodeValue | RBuilder NodeValue | BBuilder NodeValue NodeValue

childrenCount EBuilder = 0
childrenCount (LBuilder _) = 1
childrenCount (RBuilder _) = 1
childrenCount (BBuilder _ _) = 2

buildParentPromise :: TreeBuilder -> [Int -> BinaryTree] -> (Int -> BinaryTree,[Int -> BinaryTree])
buildParentPromise EBuilder childrenPromises = (Leaf, childrenPromises)

buildParentPromise (RBuilder rValue) [] = (\parentValue -> BinaryTree parentValue EmptyBinaryTree (Leaf rValue), [])
buildParentPromise (RBuilder rValue) (first:rest) = (\parentValue -> BinaryTree parentValue EmptyBinaryTree (first rValue), rest)

buildParentPromise (LBuilder lValue) [] = (\parentValue -> BinaryTree parentValue (Leaf lValue) EmptyBinaryTree, [])
buildParentPromise (LBuilder lValue) (first:rest) = (\parentValue -> BinaryTree parentValue (first lValue) EmptyBinaryTree, rest)

buildParentPromise (BBuilder lValue rValue) [] = (\parentValue -> BinaryTree parentValue (Leaf lValue) (Leaf rValue), [])
buildParentPromise (BBuilder lValue rValue) [first] = (\parentValue -> BinaryTree parentValue (first lValue) (Leaf rValue), [])
buildParentPromise (BBuilder lValue rValue) (first:second:rest) = (\  parentValue -> BinaryTree parentValue (first lValue) (second rValue), rest)

buildAll:: [TreeBuilder] -> [Int -> BinaryTree] -> [Int -> BinaryTree]
buildAll [] [] = []
buildAll (currentBuilder:nextBuilders) nextLevelPromises =
  let
    (result1,remainingPromises) = buildParentPromise currentBuilder nextLevelPromises
  in result1:buildAll nextBuilders remainingPromises

getBuilders = Data.List.map getBuilder

getBuilder (left,right)
 | left == -1 && right == -1 = EBuilder
 | left == -1 = RBuilder right
 | right == -1 = LBuilder left
 | otherwise = BBuilder left right

buildChildren:: Int -> [TreeBuilder] -> [Int -> BinaryTree]
buildChildren totalChildren [] = Data.List.replicate totalChildren Leaf
buildChildren totalChildren nodes
  | totalChildren > 0 =
      let
        (thisLevel,nextLevels) = Data.List.splitAt totalChildren nodes
        nextLevelTotalChildren = sum $ Data.List.map childrenCount thisLevel
        nextLevelChildrenPromises = buildChildren nextLevelTotalChildren nextLevels
      in buildAll thisLevel nextLevelChildrenPromises
  | otherwise = []

buildTree pairs =
  let
    [l1] = buildChildren 1 $ getBuilders pairs
  in l1 1

traverseInorder EmptyBinaryTree = []
traverseInorder (BinaryTree value left right) = traverseInorder left ++ (value:traverseInorder right)
traverseInorder (Leaf value) = [value]

swapRec:: [Int] -> Int -> BinaryTree -> BinaryTree
swapRec allLevels@(level:nextLevels) currentLevel (BinaryTree value leftChild rightChild)
  | level == currentLevel = BinaryTree value (swapRec nextLevels (currentLevel + 1) rightChild) (swapRec nextLevels (currentLevel + 1) leftChild)
  | otherwise = BinaryTree value (swapRec allLevels (currentLevel + 1) leftChild) (swapRec allLevels (currentLevel + 1) rightChild)

swapRec _ _ leaf@(Leaf _) = leaf
swapRec _ _ leaf@EmptyBinaryTree = leaf

levels k =
  let
    levelsRec level = (level*k):levelsRec (level + 1)
  in levelsRec 1

swap k = swapRec (levels k) 1

swapNTimes levels pairs = Data.List.reverse $ Data.List.map traverseInorder $ Data.List.init $ Data.List.foldl (\ t current -> swap current (Data.List.head t):t) [buildTree pairs] levels