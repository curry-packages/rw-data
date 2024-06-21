module MyFlatCurry.RWOps where

import RW.Base

writeDataFile :: ReadWrite a => String -> a -> IO ()
writeDataFile = writeDataFileP params

showData :: ReadWrite a => a -> String
showData = showDataP params

readData :: ReadWrite a => String -> Maybe a
readData = RW.Base.readData

readDataFile :: ReadWrite a => String -> IO (Maybe a)
readDataFile = RW.Base.readDataFile

params :: RWParameters 
params = RWParameters  5 26