module Test where


                                                          

maxSubsetSumRec [a] = (a,Nothing)
maxSubsetSumRec (a:rest) =  case maxSubsetSumRec rest of
                             (m1,Nothing) -> (a, Just m1)
                             (m1, Just m2) -> (maximum [a,m2,a+m2], Just (max m1 m2))                                                         

maxSubsetSum arr = case maxSubsetSumRec arr of
                          (a,Nothing) -> a
                          (a, Just b) -> max a b