{-# LANGUAGE BlockArguments #-}

module ValidBST(isValidBSTMain) where

 import Control.Monad.State
 import Data.Maybe as Maybe
 import System.IO
 import Data.List
 import Data.Char (isSpace)
 import Text.Printf (printf)

 type Min = Maybe Int
 type Max = Maybe Int
 type Parent = Int

 data TraverseData = TraverseData {
   parent :: Int,
   min ::  Maybe Int,
   max :: Maybe Int
   } deriving (Show,Eq)

 updateMin (TraverseData p _ max) = TraverseData p (Just p) max
 updateMax (TraverseData p min _) = TraverseData p min (Just p)
 updateParent  newParent (TraverseData _ min max) = TraverseData newParent min max

 updateOnTraverseLeft traverseData newParent =  updateParent newParent $ updateMax traverseData
 updateOnTraverseRight traverseData newParent = updateParent newParent $ updateMin traverseData


 canBeLeftChild (TraverseData parent (Just min) _) potentialLeftChild = potentialLeftChild < parent && potentialLeftChild > min
 canBeLeftChild (TraverseData parent Nothing _) potentialLeftChild = potentialLeftChild < parent

 canBeRightChild  (TraverseData parent _ (Just max)) potentialRightChild = potentialRightChild > parent && potentialRightChild < max
 canBeRightChild  (TraverseData parent _ Nothing) potentialRightChild = potentialRightChild > parent

 isInvalidState (TraverseData _ Nothing _) _ = False
 isInvalidState (TraverseData parent (Just min) _) potentialChild = potentialChild < parent && potentialChild > min

 myTraverse :: [Int] ->  Maybe [Int]
 myTraverse [] = Just []
 myTraverse [_] = Just []
 myTraverse (first:second:rest) =
   let
    rootData = TraverseData first Nothing Nothing
    traverseLeftFirst = (second < first)
    remainingNodes = if traverseLeftFirst then traverseRec (updateOnTraverseLeft rootData second) rest
                                          else traverseRec (updateOnTraverseRight rootData second) rest
   in
    case remainingNodes of
      Just (remainingFirst:otherRest) | traverseLeftFirst && canBeRightChild rootData remainingFirst -> traverseRec (updateOnTraverseRight rootData remainingFirst) otherRest
      _ -> remainingNodes

 traverseRightIfPossible _ [] = Just []
 traverseRightIfPossible traverseData remainingNodes@(first:_)
  | canBeRightChild traverseData first = traverseRec traverseData remainingNodes
  | otherwise = Just remainingNodes

 traverseRec :: TraverseData -> [Int] -> Maybe [Int]
 traverseRec _ [] = Just []
 traverseRec  traverseData remainingNodes@(first:rest)
   | canBeLeftChild traverseData first = traverseRec (updateOnTraverseLeft traverseData first)  rest >>= traverseRightIfPossible traverseData
   | canBeRightChild traverseData first = traverseRec (updateOnTraverseRight traverseData first) rest
   | otherwise = Just remainingNodes

 isValidBST :: [Int] -> [Char]
 isValidBST nodes = if myTraverse nodes == Just [] then "YES" else "NO"

 readIntegers :: [Char] -> [Int]
 readIntegers chars = Data.List.map (\x -> read x ::Int) $ Data.List.words chars

 isValidBSTMain :: IO()
 isValidBSTMain = do
   firstLineStr <- System.IO.getLine
   let totalScenarios = read firstLineStr :: Int
   forM_ [1..totalScenarios] $ \_-> do
     _ <- System.IO.getLine
     chars <- System.IO.getLine
     let arrayToEvaluate = readIntegers chars
     printf "%s\n" (isValidBST arrayToEvaluate)




