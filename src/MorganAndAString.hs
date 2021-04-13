module MorganAndAString 
(morganAndString)
where
    
  
morganAndString1 a [] = a
morganAndString1 [] a =  a
morganAndString1 l1@(a : rest1) l2@(b : rest2)
  | a < b = a : morganAndString1 rest1 l2
  | b < a = b : morganAndString1 l1 rest2
  | canAppend a rest1 rest2 = a : b : morganAndString1 rest1 rest2
  | otherwise = let
                  withA =  morganAndString1 rest1 l2
                  withB =  morganAndString1 l1 rest2
                in a:(if withA < withB then withA else withB)

canAppend _ _ [] = False
canAppend _ [] _ = False
canAppend current (a:_) (b:_) = a == b && current <= a 

   
morganAndString str1 str2 = morganAndString1 str1 str2


