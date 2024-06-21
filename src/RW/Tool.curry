{-# OPTIONS_FRONTENT -Wno-incomplete-patterns #-}
{-# OPTIONS_FRONTENT -Wno-name-shadowing #-}

------------------------------------------------------------------------------
--- Implementation of the command-line interface/tool and abstract RW program
--- generation.
---
--- @author Lasse Züngel
--- @version June 2024
------------------------------------------------------------------------------

module RW.Tool where

import Data.List  ( nub, (\\), intercalate, intersperse, last )
import Data.Maybe ( catMaybes )

import Control.Applicative
import Control.Monad

import qualified FlatCurry.Files as FCF
import qualified FlatCurry.Types as FCT

import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import AbstractCurry.Types

import Numeric (readNat)

import System.Environment
import System.FilePath       ( (</>) )
import System.Process
import System.CurryPath      ( lookupModuleSourceInLoadPath, runModuleAction
                             , stripCurrySuffix, splitModuleIdentifiers )
import System.Console.GetOpt

import RW.Build
import RW.Monad

--- Implementation of the abstract ReadWrite program generation tool
--------------------------------------------------------------------

--- The codegen version.
version :: Float
version = 0.1

--- Generates the ReadWrite class.
genClass :: [CFuncDecl] -> RWM CClassDecl
genClass pdcfs = do
  module' <- getModuleName
  fls <- getFunctionLayouts

  return $ CClass (module', rwClassName rwNaming) Public (CContext []) (0, "a") (map (makeFunc module') fls ++ pdcfs)
 where
  makeFunc module' (FunctionLayout name t _) = CFunc (module', name) 0 Public (CQualType (CContext []) t) []

--- Generates a ReadWrite instance for a type declaration.
--- Given a type
---   T t1 ... tn = C1 c1_1 ... c1_k1 | ... | Cn cn_1 ... cn_kn 
--- the function generates an instance
---   instance (ReadWrite t1, ..., ReadWrite tn) => ReadWrite (T t1 ... tn) where
---     read  = ...
---     write = ...
---     ...
--- The concrete read and write (and/or possibly other) function generation 
--- depends on the generator functions supplied by the concrete implementation.
genInstance :: CTypeDecl -> RWM CInstanceDecl
genInstance  t = case t of
  CType _ _ tvs _ _ -> do
    fls <- getFunctionLayouts
    funs <- mapM (genFunction t) fls

    let baseMod = rwBaseModuleName rwNaming
    return $ CInstance (baseMod, rwClassName rwNaming) (CContext (classConstraint (rwClassName rwNaming) baseMod tvs)) (typeDeclToTypeExpr t) funs
  _                 -> error "(internal) Should've been a data declaration!"

genInstances :: [CTypeDecl] -> RWM [CInstanceDecl]
genInstances tds = mapM genInstance (filter (not . isTypeSyn) tds)

--- Generates the complete RW curry program. 
gen :: RWM CurryProg
gen = do
  modname <- getModuleName
  a <- getProgram
  generatedInstances <- genInstances (types a)

  return (CurryProg modname [progName a, (rwBaseModuleName rwNaming), "System.IO"] Nothing [] generatedInstances [] [] [])

--- For a given type 't' and a function layout, this function generates a function declaration.
--- The function layout contains the name of the function, the type of the function and the
--- generator function for the function body.
genFunction :: CTypeDecl -> FunctionLayout -> RWM CFuncDecl
genFunction type' (FunctionLayout name _ genF) = do
  rs <- genF type'
  modname <- getModuleName
  return $ CFunc (modname, name) 0 Public (CQualType (CContext []) (typeDeclToTypeExpr type')) rs

--------------------------------------------------------------------------------------------
--- Analyzes the module, handles missing data definitions

--- Returns the names of all data definitions in the given program.
allDataDefs :: CurryProg -> [QName]
allDataDefs = nub . map typeName . types

--- Returns the names of all data definitions used in the program.
allDataUsed :: CurryProg -> [QName]
allDataUsed  = nub . concatMap allDataInConstructor . constructors
 where
  allDataInConstructor cons = case cons of
    (CCons   _ _ tes) -> concatMap tconsOfType tes
    (CRecord _ _ fds) -> concatMap tconsOfField fds
  tconsOfField (CField _ _ te) = tconsOfType te

--- Returns the names of all data names defined in the program. 
--- Used to retrieve the names of all predefined data definition instances.
allPredefined :: Naming -> [CInstanceDecl] -> [QName]
allPredefined (Naming _ cn _) = nub . map allPredefined' . filter ((== cn) . snd . instanceName)
 where
  instanceName (CInstance n _ _ _) = n
  allPredefined' :: CInstanceDecl -> QName
  allPredefined' (CInstance _ _ te _) = case te of
    (CTCons n) -> n
    _          -> case tconsArgsOfType te of
                    (Just (n, _)) -> n
                    Nothing       -> error $ "allPredefined: " ++ show te ++ " should've been a base type!"

--- Returns true iff the type declaration contains functional types.
containsFunction :: CTypeDecl -> Bool
containsFunction td
  = case td of
    (CType _ _ _ cds _) -> any cf_ConsDecl cds
    (CTypeSyn {})       -> False
 where
  cf_ConsDecl cd = case cd of
    (CCons _ _ tes)   -> any cf_TypeExpr tes
    (CRecord _ _ fds) -> any cf_FieldDecl fds
  cf_FieldDecl (CField _ _ te) = cf_TypeExpr te
  cf_TypeExpr te = case te of
    (CTCons _)        -> False
    (CFuncType {})    -> True
    (CTApply te1 te2) -> cf_TypeExpr te1 || cf_TypeExpr te2
    _                 -> False

--- Returns the module names of all qualified names.
modules :: [QName] -> [MName]
modules = nub . map fst

--------------------------------------------------------------------------------------------
--- CLI tool implementation

--- The default minimum string length for extraction.
defaultStrLn :: Int
defaultStrLn = 5

--- The default string id alphabet length.
defaultAlphabetLength :: Int
defaultAlphabetLength = 26

--- Runs the codegen tool for the given read and write generator as well as the format representation type.
runTool :: [String] -> [FunctionLayout] -> IO()
runTool args fls = do
  putStrLn toolBanner
  (opts, prog) <- processOptions args
  case prog of 
    [] -> putStrLn "No modules specified!\nUsage information: `--help'"
           >> exitWith 1
    ps -> do
      mapM_ (runModuleAction (tryTransform opts) . stripCurrySuffix) ps
      if optGenOpsFile opts
        then do
          putStrLn $ "\nGenerating parametrized read and write operations"
          let generatedProg = generateOperations opts
          writeProg opts "." generatedProg
          putStrLn $ "Generated module: " ++ show (progName generatedProg)
        else return ()
 where
  tryTransform opts modname =
    lookupModuleSourceInLoadPath modname >>=
    maybe (putStrLn ("Module '" ++ modname ++ "' not found!") >> exitWith 1)
          (\(dir,_) -> transform opts dir modname)

  transform opts basedir modname = do
    prog <- flatProgToAbstract <$> FCF.readFlatCurry modname
    putStrLn $ "\nGenerating ReadWrite instances for '" ++ progName prog ++ "'"
    let resultRWM = runRWM gen (Runtime (progName prog ++ "RW") fls prog [] [])
    let generatedProg = fst resultRWM

    let predefineds = [pre "Int", pre "Float", pre "[]", pre "Char", pre "()", pre "(,)", pre "(,,)", pre "(,,,)"]
    let missing = (allDataUsed prog \\ allDataDefs prog) \\ predefineds

    writeProg opts basedir generatedProg
    putStrLn $ "Data definitions generated  for: " ++  ppData (allDataDefs prog)
    putStrLn $ "Data definitions predefined for: " ++  ppData  predefineds

    unless (null missing) $
      do putStrLn $ "Missing data definitions:        " ++ show (ppData $ missing)
         putStrLn $ "Please provide the definitions for the missing data types, either by manually inserting them or by running this tool on the following module(s) and then importing the resulting module(s): "
         putStrLn $ "  " ++ show (modules missing) ++ "\n"
    let cfs = filter containsFunction (types prog)
    unless (null cfs) $
      do putStrLn $ "Warning: The following data definitions contain function types: " ++ show (ppData $ map typeName cfs)
         putStrLn   "         Functions cannot be read or written."
    unless (null $ getIllTypedDefinitions (snd resultRWM)) $
      do putStrLn $ "Warning: Typing of the following polymorphic type declarations might be incomplete: " ++ show (intercalate ", " (getIllTypedDefinitions (snd resultRWM)))
    mapM_ putStrLn (getErrors $ snd resultRWM)

  -- Writes the generated program to a file.
  -- For an input file path "a/b/foo.curry" the output file will be "a/b/fooRW.curry".
  writeProg opts basedir p = do
    let modids = splitModuleIdentifiers (progName p)
    let fn = if null (optOutDir opts)
               then (if basedir `elem` [".","./"] then id else (basedir </>))
                       (foldr1 (</>) modids ++ ".curry")
               else optOutDir opts </> last modids ++ ".curry"
    putStrLn $ "as module '" ++ progName p ++ "' stored in file '" ++
               fn ++ "'..."
    writeFile fn (showProg (addVersion p))

  ppData = intercalate ", " . map snd
  --- Adds a version function to the program if no other functions are defined. 
  --- This needs to be done as the pretty printer will only export the RW-class definiton
  --- if all (but not zero) function definitions are public.
  addVersion cp@(CurryProg n i d cs is ts fs ops)
   = case fs of
      [] -> CurryProg n i d cs is ts (CFunc (n, "version_"++ map (\c -> if c == '.' then '_' else c) n) 0 Public (CQualType (CContext []) floatType) [simpleRule [] (cFloat version)] : fs) ops
      _  -> cp
  -- 'setShowLocalSigs' is used to show the type signatures of local functions in the generated RW module. 
  -- Necessary for 'typeOf'.
  showProg = prettyCurryProg (setNoQualification (setShowLocalSigs True defaultOptions))

------------------------------------------------------------------------------
--- Command line processing

toolBanner :: String
toolBanner = "--------------------------------------\n"
          ++ "ReadWrite instance generator for Curry\n"
          ++ "--------------------------------------"

processOptions :: [String] -> IO (CLOptions, [String])
processOptions  args = do
  let (funopts, args', opterrors) = getOpt Permute options args
      dfltopts = CLOptions defaultStrLn defaultAlphabetLength "" False False
      opts     = foldl (flip id) dfltopts funopts
  unless (null opterrors) $
    putStrLn (unlines opterrors) >> printUsage >> exitWith 1
  when (optHelp opts) $ printUsage >> exitWith 0
  return (opts, args')

printUsage :: IO ()
printUsage = putStrLn $
  usageInfo "Usage: curry-rw-data [options] <module names> ..." options

options :: [OptDescr (CLOptions -> CLOptions)]
options =
  [ Option "h?"["help"]      (NoArg (\opts -> opts {optHelp=True}))
           "Print this help message"
  , Option "d" ["outdir"]    (ReqArg (\s opts -> opts { optOutDir = s }) "<d>")
           "Write generated modules into directory <d>"
  , Option "p" ["paramops"]  (NoArg (\opts -> opts { optGenOpsFile = True }))
           ("Generate module containing read/write operations\n" ++
            "with parameters (as in the subsequent options)")
  , Option "s" ["stringlen"] (ReqArg (safeReadNat checkSl) "SLEN")
           ("Minimum length of extracted strings (default: " ++
            show defaultStrLn ++ ")")
  , Option "a" ["alphabet"]  (ReqArg (safeReadNat checkAl) "ALEN")
           ("Alphabet length (default: " ++ show defaultAlphabetLength ++
            ")\nAlphabet length must be within [1..94]")
  ] 
 where
  safeReadNat opttrans s opts = case readNat s of
    [(n, "")] -> opttrans n opts
    _         -> error ("Invalid number argument: " ++ s)

  checkAl al opt 
   | al >= 1 && al <= 94 = opt {optAlphabetLength=al}
   | otherwise           = error "Alphabet length must be within [1..95]."

  checkSl sl opt 
   | sl >= 0    = opt {optStringLength=sl}
   | otherwise  = error "Minimum string length must be non-negative."

--- Based on the command line options, this function generates a module
--- containing specific parametrized versions of the write and show functions.
generateOperations :: CLOptions -> CurryProg 
generateOperations (CLOptions sl al _ _ _) =
  CurryProg (modName) [baseModName] Nothing [] [] [] fs []
 where
  fs = [cfunc  (modName, "writeDataFile") 1 Public qt1 [r1], 
        cfunc  (modName, "showData")  1 Public qt2 [r2], 
        cfunc  (modName, "readData")  1 Public qt3 [r3], 
        cfunc  (modName, "readDataFile")  1 Public qt4 [r4], 
        stCmtFunc
         ("The parameters of the show/write operations:\n" ++
          "minimum length of extract strings and alphabet length.")
         (modName, "params") 0 Public
         (baseType (baseModName, "RWParameters")) [r5]]

  context = CContext [((baseModName, "ReadWrite"), genericTypeVariable)] -- ReadWrite a => 
  qt1 = CQualType context (stringType ~> genericTypeVariable ~> ioType unitType) -- String -> a -> IO ()
  qt2 = CQualType context (genericTypeVariable ~> stringType)                    -- a -> String
  qt3 = CQualType context (stringType ~> maybeType genericTypeVariable)          -- String -> Maybe a
  qt4 = CQualType context (stringType ~> ioType (maybeType genericTypeVariable)) -- String -> IO (Maybe a)

  r1 = CRule [] $ simpleRhs (applyF (baseModName, "writeDataFileP") [CSymbol (modName, "params")])
  r2 = CRule [] $ simpleRhs (applyF (baseModName, "showDataP") [CSymbol (modName, "params")]) 
  r3 = CRule [] $ simpleRhs (applyF (baseModName, baseModName ++ "readData") [])
  r4 = CRule [] $ simpleRhs (applyF (baseModName, baseModName ++ "readDataFile") [])
  r5 = CRule [] $ simpleRhs (applyF (baseModName, "RWParameters ") [cInt sl, cInt al])
  modName     = rwParametrizedModuleName rwNaming
  baseModName = rwBaseModuleName rwNaming

--------------------------------------------------------------------------------------------
--- FunctionGenerator implementations for the RW class instances

--- hexadezimal coding
coding :: [Char]
coding = ['0'..'9'] ++ ['a'..'f']

--- Logarithm to base b
logI :: Int -> Int -> Int
logI b n | b == 1    = n
         | n <= b    = 1
         | otherwise = 1 + logI b (n `div` b)

--- Used to encode a constructor index as a list of characters (for pattern matching in the read function).
codingI :: Int -> Int -> [Char]
codingI i cs | cs == 1   = "" -- No pattern matching needed for a single-constructor type
             | otherwise = prefix ++ result
 where 
  l = logI (length coding) cs
  result = codingI' i
  prefix = replicate (l - length result) '0'

  codingI' n = (if n < length coding then "" else codingI' (n `div` length coding)) ++ [coding !! (n `mod` length coding)]

--- show generator implementation
generatorShow :: FunctionGenerator
generatorShow typedecl = return $ zipWith (forCons (rwBaseModuleName rwNaming, "showRW")) [0..] tcs
  where
    tcs = typeCons typedecl
    forCons wfn i (CCons name _ tes)
      = CRule (lhs name tes)
          (CSimpleRhs (tupleExpr [CVar (length tes, "strs" ++ show (length tes)), outputExpr]) wheres)
      where
        outputExpr | length tcs == 1 && null tes = CApply (CSymbol ("Prelude", "showString")) (string2ac "")
                   | otherwise                   = applyExpr $ optimizeSingleConstructor tcs (outputConstr : rest)
        outputConstr | length tcs <= length coding = applyF ("Prelude", "showChar")   [cChar (coding !! i)]
                     | otherwise                   = applyF ("Prelude", "showString") [string2ac (codingI i (length tcs))]
        rest = map (\index -> CVar (index, "show" ++ show (index + 1))) (fromIndex0 tes)
        wheres = map (\rn -> CLocalPat (
                                tuplePattern [CPVar (rn + 1, "strs" ++ show (rn + 1)), CPVar (rn + 1 + length tes, "show" ++ show (rn+1))]
                              ) (
                                CSimpleRhs (applyF wfn [CVar (0, "params"), CVar (rn, "strs" ++ show rn), CVar (rn + length tes, varName rn ++ "'")]) []
                              )) (fromIndex0 tes)

    lhs name tes = [CPVar (0, "params"), CPVar (0, "strs0"), CPComb name (map (\index -> CPVar (index + (length tes + 1), varName index ++ "'")) (fromIndex0 tes))]

--- read generator implementation
generatorRead :: FunctionGenerator
generatorRead typedecl = return $ zipWith (forTargetCons (rwBaseModuleName rwNaming, "readRW")) [0..] tcs
 where
  tcs = typeCons typedecl
  forTargetCons rfn i (CCons name _ tes)
    = CRule (lhs (codingI i (length tcs)))
        (CSimpleRhs (
          tupleExpr [
            applyF name (map (\index -> CVar (index + length tes, varName index ++ "'")) (fromIndex0 tes)),
            CVar (length tes, "r"++show (length tes))
          ]
        ) wheres)
   where
    wheres = map (\rn -> CLocalPat (
                            tuplePattern [CPVar (rn + length tes, varName rn ++ "'"), CPVar (rn+1, "r"++show (rn+1))]
                          ) (
                            CSimpleRhs (CApply (CApply (CSymbol rfn) (CVar (0, "strs"))) (CVar (rn, "r"++show rn))) []
                          )) (fromIndex0 tes)

  lhs cs = [CPVar (0, "strs"), listRestPattern (map pChar cs ++ [CPVar (1, "r0")])]

--- write generator implementation
generatorWrite :: FunctionGenerator
generatorWrite typedecl = return $ zipWith (forCons (rwBaseModuleName rwNaming, "writeRW")) [0..] tcs
 where
  tcs = typeCons typedecl
  forCons wfn i (CCons name _ tes)
    = CRule (lhs name tes) (CSimpleRhs rhs [])
   where
    rhs | null tes && length tcs == 1 = CApply (CSymbol $ pre "return") (CVar (length tes + 1, "strs"))
        | null tes                    = applyF (pre ">>") [writeCons, CApply (CSymbol $ pre "return") (CVar (length tes + 1, "strs"))] 
        | length tcs == 1             = monad 
        | otherwise                   = applyF (pre ">>") [writeCons, monad]
    writeCons  | length tcs <= length coding = applyF ("System.IO","hPutChar") [CVar (0,"h"), cChar (coding !! i)] 
               | otherwise                   = applyF ("System.IO","hPutStr")  [CVar (0,"h"), string2ac (codingI i (length tcs))]
    monad      = combineWithL (pre ">>=") (map (\index -> applyF wfn (CVar (0, "params") : (args index))) (fromIndex0 tes))
    args index = appendIf (index == 0) [CVar (0, "h"), CVar (index + 1, varName index ++ "'")] (CVar (length tes + 1, "strs"))
    lhs name tes = [CPVar (0, "params"), CPVar (0, "h"), CPComb name (map (\index -> CPVar (index + 1, varName index ++ "'")) (fromIndex0 tes)), CPVar (length tes + 1, "strs")]

--- typeOf generator implementation
---
--- For a data definition 
---  data T a b ... = ...
--- this function generates the following code:
---  typeOf :: T a b ... -> RWType
---  typeOf n = RWType "T" [typeOf (get_a n), typeOf (get_b n), ...]
---    where 
---      get_a :: T a b ... -> a
---      get_a (T a b ...) = failed
---      ... 
generatorTypeOf :: FunctionGenerator
generatorTypeOf typedecl = do 
  if isMonomorphic typedecl 
    then return [CRule [anonPattern]
                 (CSimpleRhs (applyF (rwbaseName "monoRWType")
                              [string2ac (snd $ typeName typedecl)]) [])]
    else return [rule]
 where
  rule = CRule [CPVar (0, "n")] (CSimpleRhs (resultExpr) getters)
  resultExpr = applyF (rwbaseName "RWType") 
                 [string2ac (snd $ typeName typedecl),
                  list2ac (map (\(_, n) -> applyF (rwbaseName "typeOf")
                                           [CApply (CSymbol $ pre $ "get_" ++ n)
                                                   (CVar (0, "n"))])
                               (typeVars typedecl))]
  getters = map getter (typeVars typedecl)
   where
    getter (_, n) = CLocalFunc (CFunc (rwbaseName $ "get_" ++ n) 1 Public
                               qual [undefRule])
     where
      qual = CQualType (CContext []) $
               applyTC (typeName typedecl)
                 (map (\(i, a) -> CTVar (i, a ++ "\'")) (typeVars typedecl))
                 ~> CTVar (length $ typeVars typedecl, n ++ "\'")
      undefRule = CRule [anonPattern] (CSimpleRhs (CSymbol $ pre "failed") [])

-- Helpers:

-- Transforms a name into a qualified name of the `RW.Base` module.
rwbaseName :: String -> QName
rwbaseName s = ("RW.Base",s)

------------------------------------------------------------------------------

--- Runs the tool
main :: IO ()
main = do
  args <- getArgs
  runWith args

runWith :: [String] -> Prelude.IO ()
runWith args = runTool args 
  [ FunctionLayout "readRW"  (listType (tupleType [stringType, stringType]) ~> stringType ~> tupleType [genericTypeVariable, stringType]) generatorRead
  , FunctionLayout "showRW"  (mapStrStr ~> genericTypeVariable ~> tupleType [mapStrStr, CTCons ("Text.Show", "ShowS")])                   generatorShow
  , FunctionLayout "writeRW" (CTCons ("System.IO", "Handle") ~> genericTypeVariable ~> mapStrStr ~> ioType mapStrStr)                     generatorWrite
  , FunctionLayout "typeOf"  (genericTypeVariable ~> CTCons (rwbaseName "RWType"))                                                  generatorTypeOf
  ] 
 where
  mapStrStr = CTApply (CTApply (CTCons ("Data.Map", "Map")) stringType) stringType
