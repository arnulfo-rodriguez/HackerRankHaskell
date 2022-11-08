{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'insertionSort' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY arr as parameter.
--    
-- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

getDigits n
  | n < 10 = [n]
  | n > 0 = let
              modulus = n `mod` 10
              divResult = n `div` 10
            in modulus : getDigits divResult

superDigit n =
  let
    firstSuperDigit = Data.List.sum (getDigits n)
  in if firstSuperDigit < 10 then firstSuperDigit else superDigit firstSuperDigit

superDigitWithList [] = 0
superDigitWithList [first] = first  
superDigitWithList (first:second:rest) = superDigit $ superDigit (first + second) + superDigitWithList rest

superDigitWithK n k =
  let 
    firstSuperDigit = superDigitWithList n
    result = superDigitWithList $ Data.List.replicate k firstSuperDigit
  in result 

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp
    let n = Data.List.map (\x -> read x :: Int) $  Data.List.map (:[]) $ firstMultipleInput !! 0
    let k = read (firstMultipleInput !! 1) :: Int
    print $ superDigitWithK n k