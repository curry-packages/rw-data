module Handle.Tests where

import Handle.MyData
import Handle.MyDataRW
import RW.Base

import BenchmarkUtils

import Data.Maybe (fromJust)
import Data.List
import System.IO
import Test.Prop
import ReadShowTerm
import System.Process ( system )

dt = genTree 13
list' = ["Prelude", "sum", "map", "x", "verylongfunctionname"]

benchmarkReadRW :: IO ()
benchmarkReadRW = do
  tree <- readFile "Handle/.curry/test.rw"
  normalform_ ((fromJust . readData) tree :: MyData MyElem)

benchmarkReadP :: IO ()
benchmarkReadP = do
  tree <- readFile "Handle/.curry/test.txt"
  normalform_ (read tree :: MyData MyElem)

benchmarkReadRST :: IO ()
benchmarkReadRST = do
  tree <- readFile "Handle/.curry/test.txt"
  normalform_ (readUnqualifiedTerm ["Handle.MyData"] tree :: MyData MyElem)

--- Output using file handle
mainHandle :: (ReadWrite a, Show a) => a -> IO ()
mainHandle val = do
  writeDataFile "Handle/.curry/test.rw" val
  writeFile "Handle/.curry/test.txt" (show val)

--- Output after building string
mainShowS :: ReadWrite a => a -> IO ()
mainShowS val = do
  writeFile "Handle/.curry/showdata.txt" $ showData val

--- Prelude output
pre :: Show a => a -> IO ()
pre val = do
  writeFile "Handle/.curry/pre.txt" $ show val

check :: PropIO
check = action `returns` True
 where 
  action = do
    system "mkdir -p Handle/.curry"
    mainHandle list'
    mainShowS list'
    a' <- readFile "Handle/.curry/test.rw"
    b' <- readFile "Handle/.curry/showdata.txt"
    return $ a' == b'
