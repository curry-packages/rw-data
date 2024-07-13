module MyFlatCurry.TestsContainer where

import MyFlatCurry.Types
import MyFlatCurry.TypesRWContainer
import MyFlatCurry.RWOps

import BenchmarkUtils

import qualified MyFlatCurry.ReadWriteBaseContainer as RWBC

import Data.List
import ReadShowTerm
import System.Directory ( removeFile )

initFCYBenchmark :: IO ()
initFCYBenchmark = do
  flat <- readFile "MyFlatCurry/Example.fcy"

  RWBC.writeDataFileP (RWBC.RWParameters  5 25 0) "MyFlatCurry/Example0.rw"  (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  RWBC.writeDataFileP (RWBC.RWParameters  5 25 1) "MyFlatCurry/Example1.rw"  (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  RWBC.writeDataFileP (RWBC.RWParameters  5 25 2) "MyFlatCurry/Example2.rw"  (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)

cleanupFCYBenchmark :: IO ()
cleanupFCYBenchmark = do
  mapM_ removeFile ["MyFlatCurry/Example0.rw", 
                    "MyFlatCurry/Example1.rw", 
                    "MyFlatCurry/Example2.rw"]

benchmarkReadRWList :: IO () 
benchmarkReadRWList = do
  flat <- readFile "MyFlatCurry/Example0.rw"
  let prog = RWBC.readData flat :: Maybe Prog
  normalform_ prog

benchmarkReadRWMap :: IO () 
benchmarkReadRWMap = do
  flat <- readFile "MyFlatCurry/Example1.rw"
  let prog = RWBC.readData flat :: Maybe Prog
  normalform_ prog

benchmarkReadRWTrie :: IO () 
benchmarkReadRWTrie = do
  flat <- readFile "MyFlatCurry/Example2.rw"
  let prog = RWBC.readData flat :: Maybe Prog
  normalform_ prog