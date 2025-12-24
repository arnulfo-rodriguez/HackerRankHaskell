module MinimumLoss
(minimumLoss) where

import Data.Maybe
import Data.List

-- | Binary Search Tree data structure
-- Each node stores:
--   - a value of type 'a'
--   - the minimum loss seen so far (Maybe a) - Nothing if no loss found yet
--   - left and right subtrees
-- NullTree represents an empty tree
data BinaryTree a = BinaryTreeNode a (Maybe a) (BinaryTree a) (BinaryTree a) | NullTree

-- | Inserts a value into the binary search tree while tracking minimum loss
-- When inserting to the left (value < existingValue):
--   - Calculates the potential loss: existingValue - value
--   - Updates minLoss with the minimum between current minLoss and the new loss
--   - This works because we're looking for the smallest positive difference where
--     the larger value (existingValue) appears before the smaller value (value) in the original sequence
-- When inserting to the right (value >= existingValue):
--   - No loss is calculated because value > existingValue means we'd be buying high and selling low
addToTree  :: (Ord a, Num a) => BinaryTree a -> a -> BinaryTree a
addToTree  NullTree value = BinaryTreeNode value Nothing NullTree NullTree
addToTree  (BinaryTreeNode existingValue minLoss leftTree rightTree) value = if value < existingValue  then
                                                                        BinaryTreeNode existingValue (minMaybe minLoss (Just (existingValue - value))) (addToTree  leftTree value) rightTree
                                                                    else
                                                                        BinaryTreeNode existingValue minLoss leftTree (addToTree  rightTree value)

-- | Builds a binary search tree from a list of values
-- Processes the list left-to-right using foldl, inserting each element into the tree
-- This maintains the temporal ordering needed to find minimum loss (buy before sell)
buildTree :: (Ord a, Num a) => [a] -> BinaryTree a
buildTree = Data.List.foldl addToTree NullTree

-- | Returns the minimum of two Maybe values
-- Handles cases where one or both values might be Nothing:
--   - If both are Nothing, returns Nothing
--   - If one is Nothing, returns the other
--   - If both have values, returns Just the minimum
minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing b = b
minMaybe a Nothing = a
minMaybe (Just a) (Just b) = Just $ min a b

-- | Traverses the binary tree to find the overall minimum loss
-- Recursively checks each node's stored minLoss and combines results from left and right subtrees
-- Returns Nothing if no valid loss was found (empty tree)
minimumLossWithTree :: (Ord a, Num a) => BinaryTree a -> Maybe a
minimumLossWithTree NullTree = Nothing
minimumLossWithTree (BinaryTreeNode _ minLoss leftTree rightTree) = minMaybe minLoss $ minMaybe (minimumLossWithTree leftTree)  $ minimumLossWithTree rightTree

-- | Main function: finds the minimum loss from a list of prices
-- The problem is to find the minimum positive difference where the larger value appears before the smaller value
-- Algorithm:
--   1. Build a BST while processing prices in order
--   2. When inserting, track losses where current price < previous prices
--   3. Extract the minimum loss from the tree
-- Returns -1 if no valid loss exists
-- Time complexity: O(n log n) average case for balanced tree, O(n^2) worst case for unbalanced
minimumLoss :: (Ord a, Num a) => [a] -> a
minimumLoss price = Data.Maybe.fromMaybe (-1) $ (minimumLossWithTree.buildTree) price
