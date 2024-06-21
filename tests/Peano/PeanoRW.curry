module Peano.PeanoRW where

import Peano.Peano
import RW.Base
import System.IO

instance ReadWrite Nat where
  readRW strs ('0' : r0) = (Zero,r0)
  readRW strs ('1' : r0) = (Successor a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 Zero = (strs0,showChar '0')
  showRW params strs0 (Successor a') = (strs1,showChar '1' . show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h Zero strs = hPutChar h '0' >> return strs
  writeRW params h (Successor a') strs =
    hPutChar h '1' >> writeRW params h a' strs

  typeOf _ = monoRWType "Nat"

version_Peano_PeanoRW :: Float
version_Peano_PeanoRW = 0.1