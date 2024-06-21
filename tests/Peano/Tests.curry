module Peano.Tests where

import Peano.Peano
import Peano.PeanoRW
import RW.Base

import BenchmarkUtils

import ReadShowTerm
import Data.Maybe 
import System.Process ( system )
import Test.Prop

initPeanoBenchmark :: Int -> IO ()
initPeanoBenchmark count = do 
  system "mkdir -p Peano/.curry"
  let nat = toNat count
  writeDataFile "Peano/.curry/Nat.rw" nat
  writeFile "Peano/.curry/Nat.t"  (show nat)

benchmarkReadRW :: IO ()
benchmarkReadRW = do
  nat <- readRW
  normalform_ nat

benchmarkReadP :: IO ()
benchmarkReadP = do
  nat <- readP
  normalform_ nat

benchmarkReadRST :: IO ()
benchmarkReadRST = do
  nat <- readT
  normalform_ nat

testCorrectness :: PropIO
testCorrectness = action `returns` True
 where 
  action = do
    initPeanoBenchmark 1000
    nat1 <- readRW
    nat2 <- readT
    return $ nat1 == nat2

----------------------------------------------

readRW :: IO Nat
readRW = do
  nat <- readFile "Peano/.curry/Nat.rw"
  return $ fromJust $ readData nat

readP :: IO Nat
readP = do
  nat <- readFile "Peano/.curry/Nat.t" 
  return $ Prelude.read nat

readT :: IO Nat
readT = do
  nat <- readFile "Peano/.curry/Nat.t"
  return $ readUnqualifiedTerm ["Peano.Peano"] nat

-- elapsed times
--  PAKCS (n=10_000):
--   Writing: 
--    rw-data:       25ms
--    ReadShowTerm: 170ms
--   Reading:
--    rw-data:       70ms
--    ReadShowTerm: 270ms
--  KiCS2 (n=1_000_000):
--   Writing:
--    rw-data:       140ms
--    ReadShowTerm:  580ms
--   Reading:
--    rw-data:        220ms
--    ReadShowTerm:  8000ms