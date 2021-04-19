{-# LANGUAGE ViewPatterns #-}

module MorganAndString
(morganAndString)
where
    
import Data.Sequence
import Data.Foldable
import Data.Maybe

naiveMorganAndString a [] = a
naiveMorganAndString [] a = a
naiveMorganAndString l1@(a : rest1) l2@(b : rest2)
  | a < b = a : naiveMorganAndString rest1 l2
  | b < a = b : naiveMorganAndString l1 rest2
  | otherwise
  = let
      withA = a : naiveMorganAndString rest1 l2
      withB = b : naiveMorganAndString l1 rest2
    in if withA < withB then withA else withB
    
suffixIsPrefix l = let indices = Data.Sequence.elemIndicesL (firstItem l) l 
                       hasGaps [] = False
                       hasGaps [_] = False
                       hasGaps (current:next:rest)  = ((current + 1) /= next) || hasGaps (next:rest)
                   in  hasGaps indices

morganAndString1 :: Seq Char -> Seq Char -> Seq Char
morganAndString1 f (Data.Sequence.viewl -> EmptyL) = f
morganAndString1 (Data.Sequence.viewl -> EmptyL) f = f
morganAndString1 l1@(Data.Sequence.viewl -> (a :< rest1)) l2@(Data.Sequence.viewl  -> (b :< rest2))
  | a < b = a <| morganAndString1 rest1 l2
  | a > b = b <| morganAndString1 l1 rest2
  | otherwise = pickNext (Data.Sequence.singleton a) rest1 rest2

lastItem sq = sq `Data.Sequence.index` (Data.Sequence.length sq - 1)
firstItem sq = sq `Data.Sequence.index` 0


pickNext :: Seq Char  -> Seq Char -> Seq Char -> Seq Char
pickNext prefix  l@(Data.Sequence.viewl -> (h1 :< r1)) r@(Data.Sequence.viewl -> (h2 :< r2))
 | h1 == h2 && (h1 <= firstItem prefix) =  pickNext (prefix |> h1)  r1 r2
 | h1 /= h2 = decideNextStep prefix l r
 | h1 > firstItem prefix = 
    if not (suffixIsPrefix prefix) 
    then 
      prefix >< prefix >< morganAndString1 l r
    else
      morganAndString1 prefix prefix >< morganAndString1 l r
      
pickNext prefix (Data.Sequence.viewl -> EmptyL) (Data.Sequence.viewl -> EmptyL) = if not (suffixIsPrefix prefix) 
                                                                                  then prefix >< prefix 
                                                                                  else let ((Data.Sequence.viewl -> (head :< tail))) = prefix
                                                                                       in head  <| morganAndString1 tail prefix
pickNext prefix (Data.Sequence.viewl -> EmptyL) b  = prefix >< morganAndString1  prefix b
pickNext prefix a (Data.Sequence.viewl -> EmptyL)   = prefix >< morganAndString1 a prefix

decideNextStep prefix l@(Data.Sequence.viewl -> (h1 :< _)) r@((Data.Sequence.viewl -> (h2 :< _)))
  | suffixIsPrefix prefix =
     if h1 < h2
      then
        firstItem prefix  <| morganAndString1 (Data.Sequence.deleteAt 0 prefix ><  l) ( prefix >< r)
      else
        firstItem prefix <| morganAndString1 ( prefix ><  l) ( Data.Sequence.deleteAt 0 prefix >< r)
  | h1 < h2 = prefix >< morganAndString1 l (prefix >< r)
  | h1 > h2 = prefix >< morganAndString1 (prefix >< l) r


morganAndString str1 str2 = Data.Foldable.toList (morganAndString1 (Data.Sequence.fromList str1) (Data.Sequence.fromList str2))
                           