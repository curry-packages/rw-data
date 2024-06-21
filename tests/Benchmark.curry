-- | This module contains a simple benchmarking tool for the compact representation.

{-# LANGUAGE CPP #-}

import qualified MyFlatCurry.Tests          as FCY
import qualified Peano.Tests                as Peano
import qualified Handle.Tests               as Handle
import qualified MyFlatCurry.TestsContainer as FCYC 

import qualified Handle.MyData

import Debug.Profile        ( getTimings )
import Control.Applicative

-- Kics2 is usually much faster than PAKCS, so identifying the system is useful
--    (benchmark sizes and/or iterations are increased, so that the benchmarks run for a reasonable amount of time).
#ifdef  __KICS2__
isKics2 :: Bool
isKics2 = True
#else
isKics2 :: Bool
isKics2 = False
#endif

--- A benchmark is a tuple of an IO action, a name and a count.
data Benchmark = Interlude 
                  { action :: IO ()
                  , name   :: String
                  }
               | Benchmark 
                  { action :: IO ()
                  , count  :: Int
                  , warmup :: Bool
                  , name   :: String
                  }

--- Creates a benchmark with a given action, count and name.
benchmark :: IO () -> Int -> String -> Benchmark
benchmark a c n = Benchmark a c True n

--- Creates a benchmark with a given action, count and name. 
--- No cache warm up is performed. Useful for benchmarks that take very long to run 
---    (usually, warming up the cache has virtually no effect on the run time).
benchmarkCold :: IO () -> Int -> String -> Benchmark
benchmarkCold a c n = Benchmark a c False n

--- Creates an interlude action. This is an action that can be run before, after or between benchmarks.
--- Useful for setting up the environment for the benchmarks, or for cleaning up afterwards.
interlude :: IO () -> String -> Benchmark
interlude = Interlude

--- Main function. Runs the profiling/benchmarking tool.
main :: IO ()
main = do
  putStrLn "Running benchmarks"
  putStrLn "=================="
  putStrLn ""
  runBenchmarks $ scale $ [ interlude     FCY.initFCYBenchmark          "Initializing FCY benchmarks..."
                          , benchmark     FCY.benchmarkReadRW       3   "FCY with rw-data" 
                          , benchmarkCold FCY.benchmarkReadP        1   "FCY with Prelude"
                          , benchmark     FCY.benchmarkReadRST      3   "FCY with ReadShowTerm"

                          , interlude     FCY.initParamBenchmark                  "Initializing parameterized FCY benchmarks..."
                          , benchmark     FCY.benchmarkParamNoStringExtraction 3  "No string extraction"
                          , benchmark     FCY.benchmarkParamNoStringInlining   3  "No string inlining"
                          , benchmark     FCY.benchmarkParamSmallAlphabet      3  "alphabet size 10 (vs 26)"
                          , benchmark     FCY.benchmarkParamBigAlphabet        3  "alphabet size 94 (vs 26)"

                          , interlude     FCYC.initFCYBenchmark         "Preparing container parameters (list, map, trie)..."
                          , benchmark     FCYC.benchmarkReadRWList   3  "FCY with parametrized containers: list" 
                          , benchmark     FCYC.benchmarkReadRWMap    3  "FCY with parametrized containers: map" 
                          , benchmark     FCYC.benchmarkReadRWTrie   3  "FCY with parametrized containers: trie" 

                          , interlude     peanoBench                    "Initializing Peano benchmarks..."
                          , benchmark     Peano.benchmarkReadRW     3   "Peano with rw-data"
                          , benchmark     Peano.benchmarkReadP      1   "Peano with Prelude"
                          , benchmark     Peano.benchmarkReadRST    3   "Peano with ReadShowTerm"

                          , interlude     binaryTree                    "Initializing Binary Tree benchmarks..."
                          , benchmark     Handle.benchmarkReadRW    3   "Binary Tree with rw-data"
                          , benchmark     Handle.benchmarkReadP     1   "Binary Tree with Prelude"
                          , benchmark     Handle.benchmarkReadRST   3   "Binary Tree with ReadShowTerm"
                          ]
 where
  peanoBench = Peano.initPeanoBenchmark (if isKics2 then 100000 else 10000)
  binaryTree = (Handle.mainHandle (Handle.MyData.genTree 12))

scale :: [Benchmark] -> [Benchmark]
scale | isKics2   = map scale'
      | otherwise = id
 where
  scale' (Interlude a n)     = Interlude a n
  scale' (Benchmark a c w n) = Benchmark a (c * kics2Scale) w n

  kics2Scale = 10

--- Runs all specified benchmarks. 
---
--- The benchmarks are run in order.
runBenchmarks :: [Benchmark] -> IO ()
runBenchmarks = mapM_ (\a -> catch (profile a) (\e -> putStrLn $ "Unexpected runtime error:" ++ show e ++ "\n"))

--- Runs a given action. If the action is a benchmark, it will run the action and handle profiling.
--- If the action is an interlude, it will run the action and print the name of the interlude.
profile :: Benchmark -> IO ()
profile (Interlude a n) = do
  putStrLn n
  a
  putStrLn ""
profile (Benchmark a c w n) = do
  let header = "Running benchmark \"" ++ n ++ "\":"
  putStrLn header
  putStrLn $ replicate (length header) '-'
  
  when w $ a

  (_, rt, et, gc) <- getTimings (sequence_ $ replicate c a)
  if c /= 1 
    then do
      putStrLn $ "Run time:     " ++ show (rt `div` c) ++ "ms per run. Total for " ++ show c ++ " runs: " ++ show rt ++ "ms"
      putStrLn $ "Elapsed time: " ++ show (et `div` c) ++ "ms per run. Total for " ++ show c ++ " runs: " ++ show et ++ "ms"
      putStrLn $ "GCs:          " ++ show (gc `div` c) ++ " per run. Total for " ++ show c ++ " runs: " ++ show gc
    else do
      putStrLn $ "Run time:     " ++ show rt ++ "ms"
      putStrLn $ "Elapsed time: " ++ show et ++ "ms"
      putStrLn $ "GCs:          " ++ show gc 
  
  putStrLn ""