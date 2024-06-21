{-
  Tests type output and type identification.
-}

module TypeIO.Tests where

import TypeIO.Defs
import TypeIO.DefsRW

import RW.Base

import System.IO
import Data.Map
import Data.Maybe
import Test.Prop
import System.Process ( system )

testDataFile :: String
testDataFile = "test.rw"

writeTest :: IO ()
writeTest =
  writeDataFile testDataFile
    ([] :: [Lots () () () () (Float, MyProblemA Int) ([Float]) (Int)])

readTest :: IO Bool
readTest = do
  val <- readDataFile testDataFile
  return $
    isJust (val :: Maybe ([Lots () () () () (Float, MyProblemA Int) ([Float]) (Int)]))

test :: PropIO
test = (writeTest >> readTest) `returns` True

testClean :: PropIO
testClean = system ("rm -f " ++ testDataFile) `returns` 0