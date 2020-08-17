module MagicSquare 
(allMagicSquares) where


newtype Matrix = Matrix [[Int]]
 deriving (Show)
data Pos = Pos Int Int
data Move = Move (Pos,Pos) Int Matrix

allEqual [] = Nothing
allEqual [f] = Just f
allEqual [f, s] = if f == s then Just f else Nothing
allEqual (f : s : rest) = if f == s then allEqual (s : rest) else Nothing

allEqualForSides f = allEqual $ map f [0 .. 2]

side (Matrix arr) = length arr

sumRow (Matrix arr) row = sum (arr !! row)

allRowsEqual m = allEqualForSides $ sumRow m

sumCol (Matrix arr) col = foldl (\acc current -> acc + (current !! col)) 0 arr

allColsEqual m = allEqualForSides $ sumCol m

sumDiag1 (Matrix arr) = foldl (\acc current -> acc + (arr !! current !! current)) 0 [0 .. (length arr - 1)]

sumDiag2 (Matrix arr) =
  let last = length arr - 1
   in foldl (\acc current -> acc + (arr !! current !! (last - current))) 0 [0 .. last]

isMagicSquare matrix =
  let allRowsEq = allRowsEqual matrix
      allColsEq = allColsEqual matrix
      d1Sum = sumDiag1 matrix
      d2Sum = sumDiag2 matrix
   in case (allRowsEq, allColsEq) of
        (Just x, Just y) -> case allEqual [x , y ,d1Sum ,d2Sum] of
                                    Just _ -> True
                                    Nothing -> False
        _ -> False


     
addToAllPositions [] e [] = [[e]]
addToAllPositions prev e [] = [prev ++ [e]]
addToAllPositions prev e arr@(head:tail) = (prev ++ (e:arr)) : addToAllPositions (prev ++ [head]) e tail

allPermutations :: [e] -> [[e]]
allPermutations  = foldl (\result current ->
                             case result of
                               [] -> [[current]] 
                               _ -> concatMap (addToAllPositions [] current) result
                        ) [] 

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list
                        
toMatrix arr = Matrix $ splitEvery 3 arr

allMagicSquares = filter isMagicSquare $ map toMatrix $ allPermutations [1,2,3,4,5,6,7,8,9] 

fixedMagicSquares = [Matrix [[4,9,2],
                             [3,5,7],
                             [8,1,6]],
                     Matrix [[2,9,4],
                             [7,5,3],
                             [6,1,8]],
                     Matrix [[2,7,6],
                             [9,5,1],
                             [4,3,8]],
                     Matrix [[6,7,2],
                             [1,5,9],
                             [8,3,4]],
                     Matrix [[4,3,8],
                             [9,5,1],
                             [2,7,6]],
                     Matrix [[8,3,4],
                             [1,5,9],
                             [6,7,2]],
                     Matrix [[8,1,6],
                             [3,5,7],
                             [4,9,2]],
                     Matrix [[6,1,8],
                             [7,5,3],
                             [2,9,4]]]
                              
distance (Matrix m) (Matrix other) =  foldl (\ d (a,b) -> d + abs(a - b)) 0  $ concatMap (uncurry zip) $  zip m other

formingMagicSquare s = minimum $ map (distance (Matrix s)) fixedMagicSquares