module BenchmarkUtils where

import Control.Applicative (when)

--- Evaluates an expression to normal form, but does not return the result.
normalform_ :: Eq a => a -> IO ()
normalform_ x = do
  when (x == x) $ return ()