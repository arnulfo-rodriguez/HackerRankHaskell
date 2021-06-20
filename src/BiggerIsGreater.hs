
module BiggerIsGreater(biggerIsGreater) where

import Data.Maybe
import Data.Sequence
import Data.Foldable

splitWhenRec _ Empty _ = Nothing
splitWhenRec c  (front :|> tl) suffix
 | c < tl = Just (tl, front, suffix)
 | otherwise = splitWhenRec c front (tl <| suffix)

swapAndSort c suffix =
  case splitWhenRec c suffix Data.Sequence.empty of
    Nothing -> Nothing
    Just (toSwap,pre,suf) -> Just (toSwap <| Data.Sequence.sort (pre ><  (c <| suf)))

biggerIsGreaterRec Empty _ = Nothing
biggerIsGreaterRec (front :|> tl) Empty = biggerIsGreaterRec front (Data.Sequence.singleton tl)
biggerIsGreaterRec (front :|> tl) suffix =
    case swapAndSort tl suffix of
      Nothing -> biggerIsGreaterRec front (tl <| suffix)
      Just something -> Just (Data.Foldable.toList (front >< something))

biggerIsGreater s = Data.Maybe.fromMaybe "no answer" (biggerIsGreaterRec  (Data.Sequence.fromList s) Data.Sequence.empty)