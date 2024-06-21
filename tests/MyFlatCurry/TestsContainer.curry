module MyFlatCurry.TestsContainer where

import MyFlatCurry.Types
import MyFlatCurry.TypesRWContainer
import MyFlatCurry.RWOps

import BenchmarkUtils

import qualified MyFlatCurry.ReadWriteBaseContainer as RWBC

import Data.List
import ReadShowTerm

initFCYBenchmark :: IO ()
initFCYBenchmark = do
  flat <- readFile "MyFlatCurry/Example.fcy"

  --- TODO: write only once, just override RWParameters  when reading
  RWBC.writeDataFileP (RWBC.RWParameters  5 25 0) "MyFlatCurry/.curry/Example0.rw"  (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  RWBC.writeDataFileP (RWBC.RWParameters  5 25 1) "MyFlatCurry/.curry/Example1.rw"  (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  RWBC.writeDataFileP (RWBC.RWParameters  5 25 2) "MyFlatCurry/.curry/Example2.rw"  (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  

benchmarkReadRWList :: IO () 
benchmarkReadRWList = do
  flat <- readFile "MyFlatCurry/.curry/Example0.rw"
  let prog = RWBC.readData flat :: Maybe Prog
  normalform_ prog

benchmarkReadRWMap :: IO () 
benchmarkReadRWMap = do
  flat <- readFile "MyFlatCurry/.curry/Example1.rw"
  let prog = RWBC.readData flat :: Maybe Prog
  normalform_ prog

benchmarkReadRWTrie :: IO () 
benchmarkReadRWTrie = do
  flat <- readFile "MyFlatCurry/.curry/Example2.rw"
  let prog = RWBC.readData flat :: Maybe Prog
  normalform_ prog