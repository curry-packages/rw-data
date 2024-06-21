--- State monad with goodies.
---
--- @author Lasse Züngel

module RW.Monad where

import Data.List

import AbstractCurry.Types
import Control.Monad 
import Control.Applicative

import RW.Build

--- Runtime data for the code generation:
data Runtime = Runtime 
  { moduleName         :: String           -- Name of the module to be generated
  , functionLayouts    :: [FunctionLayout] -- Function layouts 
  , program            :: CurryProg        -- The Curry program to be processed
  , errors             :: [String]         -- Errors that occured during the code generation
  , illTypedDefintions :: [String]         -- Definitions that could not be typed      
  }

--- Command line options:
data CLOptions = CLOptions 
  { optStringLength   :: Int    -- Minimum length of extracted strings
  , optAlphabetLength :: Int    -- Length of the string id alphabet
  , optOutDir         :: String -- output directory
  , optGenOpsFile     :: Bool   -- generate module with parameterized r/w ops?
  , optHelp :: Bool
  } deriving (Show)

newtype RWM a = RWM { runRWM :: Runtime -> (a, Runtime) }

--- A function layout describes how a function is generated.
data FunctionLayout = FunctionLayout 
  { funcName      :: String
  , funcType      :: CTypeExpr
  , funcGenerator :: FunctionGenerator
  }

--- A function generator is a function that takes a type declaration and returns the appropriate function rule(s). 
type FunctionGenerator = CTypeDecl -> RWM [CRule]

-- Naming scheme for the generated code
data Naming = Naming 
  { rwBaseModuleName         :: String
  , rwClassName              :: String
  , rwParametrizedModuleName :: String
  }

--- Default naming for the ReadWrite code generation
rwNaming :: Naming
rwNaming = Naming "RW.Base" "ReadWrite" "RWOps"

instance Functor RWM where
  fmap = liftA

instance Applicative RWM where
  pure x = RWM $ \rt -> (x,rt)
  (RWM sf) <*> (RWM sa) =  RWM $ \rt -> let (fn, rt') = sf rt
                                            (a, rt'') = sa rt'
                                        in (fn a, rt'') 

instance Monad RWM where
  return = pure
  a >>= f = RWM $ \rt -> let (x, rt') = runRWM a rt
                         in runRWM (f x) rt'

get :: RWM Runtime
get = RWM $ \rt -> (rt, rt)

put :: Runtime -> RWM ()
put rt = RWM $ \_ -> ((), rt)

getFunctionLayouts :: RWM [FunctionLayout]
getFunctionLayouts = RWM $ \rt@(Runtime{functionLayouts=fl}) -> (fl, rt)

getModuleName :: RWM String
getModuleName = RWM $ \rt@(Runtime{moduleName=mn}) -> (mn, rt)

getProgram :: RWM CurryProg
getProgram = RWM $ \rt@(Runtime{program=p}) -> (p, rt)

logIllTypedDefinition :: String -> RWM ()
logIllTypedDefinition def = RWM $ \rt@(Runtime{illTypedDefintions=defs}) -> ((), rt { illTypedDefintions = def:defs })

getIllTypedDefinitions :: Runtime -> [String]
getIllTypedDefinitions (Runtime{illTypedDefintions=defs}) = nub defs

logError :: String -> RWM ()
logError err = RWM $ \rt@(Runtime{errors=errs}) -> ((), rt { errors = err:errs })

getErrors :: Runtime -> [String]
getErrors (Runtime{errors=errs}) = errs