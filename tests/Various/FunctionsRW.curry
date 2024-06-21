module Various.FunctionsRW where

import Various.Functions
import RW.Base
import System.IO

instance ReadWrite MyData where
  readRW strs r0 = (MyFunctionWrapper a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 (MyFunctionWrapper a') = (strs1,show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h (MyFunctionWrapper a') strs = writeRW params h a' strs

  typeOf _ = monoRWType "MyData"

version_Various_FunctionsRW :: Float
version_Various_FunctionsRW = 0.1