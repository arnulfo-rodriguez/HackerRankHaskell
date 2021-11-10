module AbsolutePermutation where


import Data.Sequence as Seq
import Data.Maybe as Maybe
import Data.Foldable as Foldable
  
possible k n = map (\ i -> (if abs i <= k then Nothing else Just (abs (k - i)), if (k + i) <= n then Just (k+i) else Nothing) ) [1..n]

absolutePermutation 0 n = [1..n]
absolutePermutation k n  =
  let possibleValues = possible k n
      absolutePermutationRec [] currentAbsolutePermutation = Just currentAbsolutePermutation
      absolutePermutationRec (possiblePair:remainingPairs) currentAbsolutePermutation = 
         case possiblePair of
              (Nothing , Just x) -> if Maybe.isNothing (x `Seq.elemIndexL` currentAbsolutePermutation) then absolutePermutationRec remainingPairs (currentAbsolutePermutation |> x) else Nothing
              (Just x, Nothing) ->  if Maybe.isNothing (x `Seq.elemIndexL` currentAbsolutePermutation) then absolutePermutationRec remainingPairs (currentAbsolutePermutation |> x) else Nothing
              (Just x, Just y) ->  if Maybe.isNothing (x `Seq.elemIndexL` currentAbsolutePermutation) 
                                       then 
                                         case absolutePermutationRec remainingPairs (currentAbsolutePermutation |> x) of
                                           Nothing -> if Maybe.isNothing (y `Seq.elemIndexL` currentAbsolutePermutation) then absolutePermutationRec remainingPairs (currentAbsolutePermutation |> y) else Nothing
                                           result -> result
                                       else if Maybe.isNothing (y `Seq.elemIndexL` currentAbsolutePermutation)
                                            then  absolutePermutationRec remainingPairs (currentAbsolutePermutation |> y)
                                            else Nothing
              _ -> Nothing
  in case absolutePermutationRec possibleValues Seq.empty of
       (Just l) -> Foldable.toList l
       Nothing -> [(-1)]                               