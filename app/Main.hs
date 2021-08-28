{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances, LambdaCase #-}

module Main where
import GHC.Stack
import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set as Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Sequence as Seq
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Set as Set
import Data.List
import Data.Function as Function
import FrogInMaze

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: HasCallStack => IO()
main = do 
    firstMultipleInputTemp <- getLine
    
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    let k = read (firstMultipleInput !! 2) :: Int

 
    let ioRows =  forM [1..n] $ \_-> do
                               line <- getLine
                               return (Data.List.map newCell $ rstrip line)
               
   
    cells <- ioRows
    let maze = buildMaze cells           
   
    tunnels <- forM [1..k] $ \_ -> do
                    secondMultipleInputTemp <- getLine
                    let secondMultipleInput = Data.List.words $ rstrip secondMultipleInputTemp

                    let i1 = read (secondMultipleInput !! 0) :: Int

                    let j1 = read (secondMultipleInput !! 1) :: Int

                    let i2 = read (secondMultipleInput !! 2) :: Int

                    let j2 = read (secondMultipleInput !! 3) :: Int

                    return (i1,j1,i2,j2)
                    
    print (fromStartToExit $ buildProbabilitiesGraph $ Data.List.foldl (\ m (i1,j1,i2,j2) -> addTunnel m i1 j1 i2 j2) maze tunnels)

