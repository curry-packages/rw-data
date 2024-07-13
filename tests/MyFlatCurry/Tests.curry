module MyFlatCurry.Tests where

import Data.List
import ReadShowTerm
import Test.Prop

import System.Process ( system )
import System.Directory ( removeFile )

import MyFlatCurry.Types
import MyFlatCurry.TypesRW
import MyFlatCurry.RWOps
import RW.Base (RWParameters (..), writeDataFileP)

import BenchmarkUtils

--------------------------------------------------------
-- Testing string extraction RWParameters 

initParamBenchmark :: IO ()
initParamBenchmark = do
  flat <- readFile "MyFlatCurry/Example.fcy"
  let prog = (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  writeDataFileP (RWParameters  1000 26) "MyFlatCurry/paramNoStringExtraction.rw" prog
  writeDataFileP (RWParameters  0 26)    "MyFlatCurry/paramNoStringInlining.rw"   prog
  writeDataFileP (RWParameters  5 10)    "MyFlatCurry/paramSmallAlphabet.rw"      prog
  writeDataFileP (RWParameters  5 94)    "MyFlatCurry/paramBigAlphabet.rw"        prog

cleanupParamBenchmark :: IO ()
cleanupParamBenchmark = do
  mapM_ removeFile ["MyFlatCurry/paramNoStringExtraction.rw", 
                    "MyFlatCurry/paramNoStringInlining.rw", 
                    "MyFlatCurry/paramSmallAlphabet.rw", 
                    "MyFlatCurry/paramBigAlphabet.rw"]
  
benchmarkParamNoStringExtraction :: IO ()
benchmarkParamNoStringExtraction = do
  flat <- readFile "MyFlatCurry/paramNoStringExtraction.rw"
  let prog = readData flat :: Maybe Prog
  normalform_ prog

benchmarkParamNoStringInlining :: IO ()
benchmarkParamNoStringInlining = do
  flat <- readFile "MyFlatCurry/paramNoStringInlining.rw"
  let prog = readData flat :: Maybe Prog
  normalform_ prog

benchmarkParamSmallAlphabet :: IO ()
benchmarkParamSmallAlphabet = do
  flat <- readFile "MyFlatCurry/paramSmallAlphabet.rw"
  let prog = readData flat :: Maybe Prog
  normalform_ prog

benchmarkParamBigAlphabet :: IO ()
benchmarkParamBigAlphabet = do
  flat <- readFile "MyFlatCurry/paramBigAlphabet.rw"
  let prog = readData flat :: Maybe Prog
  normalform_ prog

--------------------------------------------------------
-- Normal flatcurry benchmarks and tests

initFCYBenchmark :: IO ()
initFCYBenchmark = do
  flat <- readFile "MyFlatCurry/Example.fcy"
  writeDataFile "MyFlatCurry/Example.rw"
    (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)

cleanupFCYBenchmark :: IO ()
cleanupFCYBenchmark = removeFile "MyFlatCurry/Example.rw"

-- PAKCS:
-- elapsed pakcs (map):  1940ms
-- elapsed pakcs (trie): 1400ms
-- KiCS2: 
-- elapsed pakcs (trie): 0.19s 
benchmarkWriteRW :: IO () 
benchmarkWriteRW = do
  input <- readFile "MyFlatCurry/Example.fcy"
  let theProgram = (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] input :: Prog)
  writeDataFile "MyFlatCurry/outputs/Example.rw" theProgram

-- elapsed pakcs: 800ms
-- elapsed kics2: 0.17s
benchmarkWriteP :: IO () 
benchmarkWriteP = do
  input <- readFile "MyFlatCurry/Example.fcy"
  let theProgram = (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] input :: Prog)
  writeFile "MyFlatCurry/outputs/Example.fcy" (show theProgram)

-- elapsed pakcs: 670ms
-- elapsed kics2: 0.17s
benchmarkWriteRST :: IO ()
benchmarkWriteRST = do
  input <- readFile "MyFlatCurry/Example.fcy"
  let theProgram = (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] input :: Prog)
  writeFile "MyFlatCurry/outputs/ExampleUnqual.fcy" (showTerm theProgram) 

--------------------------------------------------------

-- (With map read:                       1500ms)
-- Using list strs and Prelude escaping: 1000ms
-- Using efficient escaping:              910ms 
--  + directly encoding small strings:    860ms
benchmarkReadRW :: IO () 
benchmarkReadRW = do
  flat <- readFile "MyFlatCurry/Example.rw"
  let prog = readData flat :: Maybe Prog
  normalform_ prog

benchmarkLoadRW :: IO ()
benchmarkLoadRW = do 
  flat <- readDataFile "MyFlatCurry/Example.rw" :: IO (Maybe Prog)
  normalform_ flat

benchmarkReadP :: IO () 
benchmarkReadP = do
  flat <- readFile "MyFlatCurry/Example.fcy"
  let prog = (read flat :: Prog)
  normalform_ prog

benchmarkReadRST :: IO () 
benchmarkReadRST = do
  flat <- readFile "MyFlatCurry/Example.fcy"
  let prog = (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flat :: Prog)
  normalform_ prog

testEquivalence :: PropIO
testEquivalence = action `returns` True
 where
  action = do
    initFCYBenchmark
    flatRW <- readFile "MyFlatCurry/Example.rw"
    flatPre <- readFile "MyFlatCurry/Example.fcy"
    let progRW = readData flatRW :: Maybe Prog
    let progPre = (readUnqualifiedTerm ["Prelude", "MyFlatCurry.Types"] flatPre :: Prog)
    case progRW of 
      Nothing -> return False
      Just val -> return $ val == progPre

testCleanEquivalence :: PropIO
testCleanEquivalence = system "rm -f MyFlatCurry/Example.rw" `returns` 0

-- Pakcs Performance:
-- ------------------
-- checkIdRW:       1230ms,         1280ms
--   using Trie:                    1170ms
--     + using a..z:                1140ms
--       + using faster int parsing  630ms
-- checkIdPrelude: 12400ms (10x)
-- checkIdUnqual:    370ms (0.3x)

-- Kics2 Performance:
-- ------------------
-- checkIdRW:       0.025s
-- checkIdPrelude:  0.400s (16x)
-- checkIdUnqual:   0.110s (4.4x)
--
-- For big inputs such as Prelude.fcy, using 'Map String String' when reading is ~20% faster than using '[(String, String)]' (same for pakcs). 
-- For smaller inputs, using the list implementation is far superior (**depending on the amount of strings in the input**). 
--  It takes pakcs 1.5x as long to checkIdRW ReadWriteBase.rw compared to the list implementation. 
--
-- -- Testing tries and faster int parsing:
-- checkIdUnqual: 0.17s
-- checkIdRW:     0.01s (x17 ?) 
--
-- checkIdUnqual: 17.3s 
-- checkIdRW:      1.8s (x9.6)
