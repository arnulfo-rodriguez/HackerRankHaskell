{-# LANGUAGE FlexibleContexts #-}

module NonDivisibleSubSet
  ( nonDivisibleSubset
  ) where
    
import qualified Data.List
import qualified Data.Function

data BinaryTree a = BinaryTreeNode a (BinaryTree a) (BinaryTree a) | NullTree deriving (Eq,Show)

addToTree  :: (Ord a) => BinaryTree a -> a -> BinaryTree a
addToTree  NullTree value = BinaryTreeNode value NullTree NullTree
addToTree  t@(BinaryTreeNode existingValue leftTree rightTree) value =  if value == existingValue then
                                                                           t
                                                                        else 
                                                                          if value < existingValue  then
                                                                            BinaryTreeNode existingValue (addToTree  leftTree value) rightTree
                                                                          else
                                                                            BinaryTreeNode existingValue leftTree (addToTree  rightTree value)
removeFromTree:: (Ord a) => BinaryTree a -> a -> BinaryTree a                                                                            
removeFromTree NullTree _ = NullTree
removeFromTree (BinaryTreeNode v l r) toRemove 
 | v == toRemove = foldl addToTree l r
 | True = (BinaryTreeNode v (removeFromTree l toRemove) (removeFromTree r toRemove))
 
treeElem :: (Ord a) =>  a -> (BinaryTree a )-> Bool
treeElem _ NullTree = False
treeElem toFind (BinaryTreeNode value leftTree rightTree)  
 | (value == toFind) = True
 | (toFind < value) =  toFind `treeElem` leftTree 
 | True = toFind `treeElem` rightTree
 
treeMerge t1 t2 = Data.List.foldl addToTree t1 t2 


instance Foldable BinaryTree where
  foldMap f NullTree = mempty
  foldMap f (BinaryTreeNode v l r) = (f v) `mappend` (foldMap f l) `mappend` (foldMap f r)

data ASet = TheSet Int Int Bool (BinaryTree Int) [Int] | NullSet Int deriving (Eq,Show)

instance Ord ASet where
  (<=) (TheSet _ len1 _ _ _) (TheSet _ len2 _ _ _) = len1 <= len2
  

mergeSorted l1 [] = l1
mergeSorted [] l2 = l2  
mergeSorted l1@(h1:v1) l2@(h2:v2) = if h1 > h2 then h1:mergeSorted v1 l2 else h2:mergeSorted l1 v2

addNextValue value (NullSet k)
   | ((value `mod` k) == 0) = [(TheSet k 1 True NullTree [])]
   | True =  [(TheSet k 1 False (addToTree NullTree (value `mod` k)) [value])]

addNextValue value initialSet@(TheSet k len hasMultipleOfK modules values) 
 | (value < k) && Data.List.all (\ v -> (v + value) > k && ((v + value) `mod` k) /= 0) values = [(TheSet k (len + 1) hasMultipleOfK (addToTree modules value) (value:values)),initialSet]
 | ((value `mod` k) == 0) && (not hasMultipleOfK) = [(TheSet k (len + 1) True modules values)]
 | ((value `mod` k) == 0) = [initialSet]
 | (value > k) && not ((k - (value `mod` k)) `treeElem` modules) =  [(TheSet k (len + 1) hasMultipleOfK (addToTree modules (value `mod` k) ) (value:values))]
 | True = let newValues = Data.List.filter (\x -> (x `mod` k) /= ((k - (value `mod` k)))) values
            in mergeSorted [initialSet] [(TheSet k ((length newValues) + 1) hasMultipleOfK (removeFromTree modules ((k - (value `mod` k)))) (value:newValues))]

allSets :: Int -> [Int] -> [ASet]
allSets k [] = []
allSets k [x] = addNextValue x (NullSet k)
allSets k (h:rest) = let prevSets = allSets k rest
                     in  Data.List.foldl (\ acc s -> mergeSorted (addNextValue h s) acc) [] prevSets
 
 
nonDivisibleSubset k s = case allSets k s of
                         [] -> 0
                         ((TheSet _ len _ _ _):_) -> len 
