module OrganizingContainers where

import Data.List
organizingContainers:: [[Int]] -> String
organizingContainers container = 
   let itemsPerContainer = sort $ map sum container
       totalPerColor = sort $ map sum $ Data.List.transpose container 
   in if totalPerColor == itemsPerContainer
       then "Possible"
       else "Impossible"