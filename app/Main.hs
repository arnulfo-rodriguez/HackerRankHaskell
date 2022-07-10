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
import InsertionSort

--
-- Complete the 'insertionSort' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY arr as parameter.
--    
-- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    fptr <- openFile "output.txt" WriteMode

    tTemp <- getLine
    let t = read $ lstrip $ rstrip tTemp :: Int

    forM_ [1..t] $ \t_itr -> do
        nTemp <- getLine
        let n = read $ lstrip $ rstrip nTemp :: Int

        arrTemp <- getLine

        let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

        let result = insertionSort arr

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr