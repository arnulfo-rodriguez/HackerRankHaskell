{-# LANGUAGE BlockArguments #-}

module ValidBST(isValidBSTMain) where

iimport Control.Monad.State
 import Data.Maybe as Maybe
 import System.IO
 import Data.List
 import Data.Char (isSpace)
 import Text.Printf (printf)

 myTraverse :: [Int] ->  Maybe [Int]
 myTraverse (first:second:rest)
  | second < first = traverseLeft second rest >>= traverseRight first
  | second > first = traverseRight first (second:rest)
 myTraverse _ = Just []

 traverseLeft :: Int -> [Int] -> Maybe [Int]
 traverseLeft _ [] = Just []
 traverseLeft parent [last] = if last < parent then Just [] else Just [last]
 traverseLeft  parent remainingNodes@(first:second:rest)
   | first < parent = traverseLeft second rest >>= traverseRight first
   | second > first && second < parent = traverseRight first (second:rest)
   | otherwise = Just remainingNodes

 traverseRight :: Int -> [Int] -> Maybe [Int]
 traverseRight  _ [] = Just []
 traverseRight parent [last] = if last > parent then Just [] else Nothing
 traverseRight parent (first:second:rest)
   | first < parent || second < parent = Nothing
   | second < first = traverseLeft first (second:rest) >>= traverseRight first
   | second > first && second > parent = traverseRight first (second:rest)

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




