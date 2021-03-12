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
removeFromTree:: (Ord a) => BinaryTree a -> a -> Maybe (BinaryTree a , a)                                                                           
removeFromTree NullTree _ =  Nothing
removeFromTree (BinaryTreeNode v l r) toRemove 
 | v == toRemove = Just (foldl addToTree l r, v)
 | True = case ((removeFromTree l toRemove) , (removeFromTree r toRemove)) of
                  (Nothing, Just(rightTree, removed)) -> Just((BinaryTreeNode v l rightTree), removed)
                  (Just(leftTree, removed),Nothing) -> Just((BinaryTreeNode v leftTree r), removed)
                  (Nothing, Nothing) -> Nothing
                    
 
treeElem :: (Ord a) =>  a -> (BinaryTree a )-> Maybe a
treeElem _ NullTree = Nothing
treeElem toFind (BinaryTreeNode value leftTree rightTree)  
 | (value == toFind) = Just value
 | (toFind < value) =  toFind `treeElem` leftTree 
 | True = toFind `treeElem` rightTree
 
treeMerge t1 t2 = Data.List.foldl addToTree t1 t2 

instance Foldable BinaryTree where
  foldMap f NullTree = mempty
  foldMap f (BinaryTreeNode v l r) = (f v) `mappend` (foldMap f l) `mappend` (foldMap f r)

-- KModSet k module length values
data KModSet = KModSet Int Int Int [Int] deriving(Show)

kmodSetMerge s1@(KModSet k1 m1 l1 values1) s2@(KModSet k2 m2 l2 values2)
 | s1 == s2 = KModSet k1 m1 (l1 + l2) (values1 ++ values2)
 | True = error "cannot merge" 

instance Eq KModSet where
  (==) (KModSet k1 mod1 _ _ ) (KModSet k2 mod2 _ _ ) = k1 == k2 && mod1 == mod2
  
instance Ord KModSet where
  (<=) (KModSet _ mod1 _ _ ) (KModSet _ mod2 _ _ ) = mod1 <= mod2


data ASet = TheSet Int Int Bool (BinaryTree KModSet)  | NullSet Int deriving (Eq,Show)

getValues l = concatMap (\ (KModSet _ _ _ values) -> values) l

addToKModSetTree :: (BinaryTree KModSet) -> KModSet -> (BinaryTree KModSet)
addToKModSetTree tree value = case removeFromTree tree value of
                              Nothing -> addToTree tree value
                              Just(newTree, kmod) -> addToTree newTree (kmodSetMerge kmod value)
                                                      
kmodSetTreeLength :: (BinaryTree KModSet) -> Int
kmodSetTreeLength tree = foldl (\ acc (KModSet _ _ l _) -> acc + l ) 0  tree

instance Ord ASet where
  (<=) (TheSet _ len1 _ _) (TheSet _ len2 _ _) = len1 <= len2
  

mergeSorted l1 [] = l1
mergeSorted [] l2 = l2  
mergeSorted l1@(h1:v1) l2@(h2:v2) = if h1 > h2 then h1:mergeSorted v1 l2 else h2:mergeSorted l1 v2

addNextValue value (NullSet k)
   | ((value `mod` k) == 0) = [(TheSet k 1 True NullTree)]
   | True =  [TheSet k 1 False (addToKModSetTree NullTree (KModSet k (value `mod` k) 1 [value]))]

addNextValue value initialSet@(TheSet k len hasMultipleOfK modules) =
  let toAdd = (KModSet k (value `mod` k) 1 [value])
  in
  if (value < k) && Data.List.all (\ v -> (v + value) > k && ((v + value) `mod` k) /= 0) (getValues modules) then
    [(TheSet k (len + 1) hasMultipleOfK (addToKModSetTree modules toAdd)),initialSet]
  else 
    if ((value `mod` k) == 0) then
      if (not hasMultipleOfK) then [(TheSet k (len + 1) True modules)] else [initialSet]
    else  case removeFromTree modules (KModSet k ((k - (value `mod` k))) 0 []) of
              Nothing -> [TheSet k (len + 1) hasMultipleOfK (addToKModSetTree modules toAdd)] 
              Just(newTree,_) -> let treeWithNewElem = (addToKModSetTree newTree toAdd) 
                                 in mergeSorted [initialSet] [TheSet k (kmodSetTreeLength treeWithNewElem) hasMultipleOfK treeWithNewElem]

  
allSets :: Int -> [Int] -> [ASet]
allSets k [] = []
allSets k [x] = addNextValue x (NullSet k)
allSets k (h:rest) = let prevSets = allSets k rest
                     in  Data.List.foldl (\ acc s -> mergeSorted (addNextValue h s) acc) [] prevSets
 
 
nonDivisibleSubset k s = case allSets k s of
                         [] -> 0
                         ((TheSet _ len _ _ ):_) -> len 

