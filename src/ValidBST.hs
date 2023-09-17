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

 updateMin (TraverseData p _ max) newMin = TraverseData p newMin max
 updateMax (TraverseData p min _) = TraverseData p min
 updateParent (TraverseData _ min max) newParent = TraverseData newParent min max

 canBeLeftChild (TraverseData parent (Just min) _) potentialLeftChild = potentialLeftChild < parent && potentialLeftChild > min
 canBeLeftChild (TraverseData parent Nothing _) potentialLeftChild = potentialLeftChild < parent

 canBeRightChild  (TraverseData parent _ (Just max)) potentialRightChild = potentialRightChild > parent && potentialRightChild < max
 canBeRightChild  (TraverseData parent _ Nothing) potentialRightChild = potentialRightChild > parent


 myTraverse :: [Int] ->  Maybe [Int]
 myTraverse (first:rest) = traverseLeft (TraverseData first Nothing Nothing) rest >>= traverseRight (TraverseData first Nothing Nothing)

 traverseLeft :: TraverseData -> [Int] -> Maybe [Int]
 traverseLeft _ [] = Just []
 traverseLeft  traverseData remainingNodes@(first:rest)
   | canBeLeftChild traverseData first = traverseLeft (updateParent (updateMax traverseData (Just first)) first)  rest >>= traverseRight (updateParent (updateMin traverseData (Just first)) first)
   | canBeRightChild traverseData first = traverseRight (updateParent (updateMin traverseData (Just first)) first) rest
   | otherwise = Just remainingNodes

 traverseRight :: TraverseData -> [Int] -> Maybe [Int]
 traverseRight  _ [] = Just []
 traverseRight traverseData (first:rest)
   | canBeRightChild traverseData first = traverseRight (updateParent (updateMin traverseData (Just first)) first) rest
   | canBeLeftChild traverseData first = traverseLeft (updateParent (updateMax traverseData (Just first)) first)  rest
   | otherwise = Nothing

 isValidBST :: [Int] -> [Char]
 isValidBST nodes = if myTraverse nodes == Just [] then "YES" else "NO"

 readIntegers :: [Char] -> [Int]
 readIntegers chars = Data.List.map (\x -> read [x] ::Int) $ Data.List.filter (not . isSpace) chars

 isValidBSTMain :: IO()
 isValidBSTMain = do
   firstLineStr <- System.IO.getLine
   let totalScenarios = read firstLineStr :: Int
   forM_ [1..totalScenarios] $ \_-> do
     _ <- System.IO.getLine
     chars <- System.IO.getLine
     let arrayToEvaluate = readIntegers chars
     printf "%s\n" $ isValidBST arrayToEvaluate




