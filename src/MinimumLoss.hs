module MinimumLoss
(minimumLoss) where

import Data.Maybe
import Data.List

data BinaryTree a = BinaryTreeNode a (Maybe a) (BinaryTree a) (BinaryTree a) | NullTree
addToTree  :: (Ord a, Num a) => BinaryTree a -> a -> BinaryTree a
addToTree  NullTree value = BinaryTreeNode value Nothing NullTree NullTree
addToTree  (BinaryTreeNode existingValue minLoss leftTree rightTree) value = if value < existingValue  then
                                                                        BinaryTreeNode existingValue (minMaybe minLoss (Just (existingValue - value))) (addToTree  leftTree value) rightTree
                                                                    else
                                                                        BinaryTreeNode existingValue minLoss leftTree (addToTree  rightTree value)

buildTree :: (Ord a, Num a) => [a] -> BinaryTree a
buildTree = Data.List.foldl addToTree NullTree

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just $ min a b

minimumLossWithTree :: (Ord a, Num a) => BinaryTree a -> Maybe a
minimumLossWithTree NullTree = Nothing
minimumLossWithTree (BinaryTreeNode _ minLoss leftTree rightTree) = minMaybe minLoss $ minMaybe (minimumLossWithTree leftTree)  $ minimumLossWithTree rightTree

minimumLoss :: (Ord a, Num a) => [a] -> a
minimumLoss price = Data.Maybe.fromMaybe (-1) $ (minimumLossWithTree.buildTree) price
