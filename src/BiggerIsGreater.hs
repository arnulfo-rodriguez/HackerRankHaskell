{-|
Module      : BiggerIsGreater
Description : Solution for finding the lexicographically next permutation

== Problem Statement

Given a string (word), find the lexicographically smallest string that is 
greater than the given string using the same characters. If no such 
rearrangement exists (i.e., the string is already the largest permutation), 
return "no answer".

Example:
- "ab" → "ba"
- "bb" → "no answer"
- "hefg" → "hegf"
- "dhck" → "dhkc"
- "dkhc" → "hcdk"

== Algorithm: Next Permutation

The solution implements the standard next permutation algorithm:

1. Find the rightmost pair (i, i+1) where s[i] < s[i+1]
   - This is the "pivot" position that needs to change
   - If no such pair exists, the string is already in descending order (largest permutation)

2. Find the smallest element in the suffix (right of pivot) that is larger than s[i]
   - This element will be swapped with the pivot

3. Swap the pivot with this element

4. Sort the remaining suffix in ascending order
   - Since the suffix was in descending order before the swap, we just need to reverse/sort it

Example walkthrough for "hefg":
1. Find pivot: "he[f]g" - 'e' < 'f', so pivot is at 'e'
2. Find successor in "fg": 'g' is the smallest element > 'e'
3. Swap: "hgf[e]"
4. Sort suffix "fe" → "ef": Result = "hgef"

Wait, let me reconsider: "hefg" → "hegf"
1. Pivot: 'f' < 'g', so 'f' is the pivot (not 'e')
2. Suffix is just "g"
3. Swap 'f' and 'g': "hegg"... that's not right.

Let me trace through again: "hefg"
- Scan from right: g > f? No. f > e? Yes. So pivot is 'e' (position where value < next)
- Suffix is "fg"
- Find smallest in suffix > 'e': that's 'f'
- Swap: "hf[e]g" → "hfeg"
- Sort suffix "eg" → "eg"... still not "hegf"

Actually looking at "hefg" → "hegf":
- From right: is g > f? Yes! So we found 'f' < 'g' 
- Actually, we need to find from RIGHT where s[i] < s[i+1]
- "hefg": scan backwards for i where s[i] < s[i+1]
  - Position 2: 'f' < 'g' ✓
- Pivot is 'f' at position 2
- Suffix is "g"  
- Find smallest element in suffix > 'f': that's 'g'
- Swap 'f' and 'g': "hegg"... no that's wrong

Hmm, I think I'm misunderstanding. Let me look at the code more carefully.

The algorithm scans from left to right building a front and suffix. When it finds a position where it can do swapAndSort successfully, it returns that.

Actually, looking at the code:
- It processes from right to left (front :|> tl means we're taking elements from the right)
- For each position, it tries swapAndSort with the suffix
- swapAndSort looks for the first element in suffix (from right) that is greater than the current element
- It swaps them and sorts the remainder

This IS the next permutation algorithm.

== Implementation Details

The implementation uses Data.Sequence for efficient operations:

- `biggerIsGreaterRec`: Recursively processes the string from right to left
  - Maintains a 'front' (unprocessed) and 'suffix' (processed) using Sequence
  - For each position, attempts to find a valid swap in the suffix
  
- `swapAndSort`: Given a character and a suffix, finds the next permutation
  - Uses `splitWhenRec` to find the smallest element in suffix > current character
  - Swaps these elements and sorts the remaining suffix
  - Returns Nothing if no valid swap exists (suffix is in descending order)

- `splitWhenRec`: Scans the suffix from right to left to find the first element > current
  - This ensures we get the smallest valid element to swap with

The algorithm processes right-to-left, trying to find the rightmost position 
where we can create a lexicographically larger permutation.

Time Complexity: O(n²) worst case due to sorting, but typically O(n)

-}
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