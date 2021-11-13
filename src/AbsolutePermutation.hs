module AbsolutePermutation where


import Data.Sequence as Seq
import Data.Foldable as Foldable
import Control.Monad
  
absolutePermutation n 0 = [1..n]
absolutePermutation n k
 | (n `mod` (k * 2)) /= 0 = [-1]
 | otherwise = let chunks = Seq.chunksOf (2*k) (Seq.fromList [1..n])
                   reverseSubChunk _ chunk =  Control.Monad.join (Seq.reverse (Seq.chunksOf k chunk))
               in  Foldable.toList $  Control.Monad.join (Seq.mapWithIndex reverseSubChunk chunks)