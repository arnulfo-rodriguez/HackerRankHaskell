{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Sequence as Seq
import SuperQueens

main :: IO()
main = do
    sizeStr <- getLine
    let size =  read  sizeStr :: Int
    print (countSuperQueens size)
