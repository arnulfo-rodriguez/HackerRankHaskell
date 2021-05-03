module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

import ConnectedCells

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    mTemp <- getLine
    let m = read $ lstrip $ rstrip mTemp :: Int

    matrixTemp <- readMultipleLinesAsStringArray n
    let matrix = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) matrixTemp

    let result = connectedCell matrix

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
