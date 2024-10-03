------------------------------------------------------------------------------
--- This library defines the class `ReadWrite` which is the basis to define
--- compact data representations for various types.
--- Furthermore, instances of this class are defined for various standard types
--- defined in the prelude.
---
--- @author Lasse Züngel
--- @version July 2024
------------------------------------------------------------------------------
{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-unused-bindings #-}

module RW.Base
  ( ReadWrite(..), RWParameters (..), defaultParams, RWType(..), monoRWType
  , readDataFile, writeDataFile, writeDataFileP, readData, showData, showDataP)
 where

import Data.Maybe ( fromJust )
import Data.List  ( intercalate, sortBy )
import System.IO  ( Handle, IOMode(WriteMode), hClose, hPutChar, hPutStr, openFile )
import Text.Show  ( ShowS, showChar, showString, shows )

import Prelude hiding (ShowS, showString, showChar, shows)

import Data.Trie as T 

------------------------------------------------------------------------------

--- The class `ReadWrite` contains the interface to be implemented
--- by compact readers and writers of data.
class ReadWrite a where
  readRW :: Trie String -> String -> (a, String)

  showRW :: RWParameters -> Trie String -> a -> (Trie String, ShowS)

  writeRW :: RWParameters -> Handle -> a -> Trie String -> IO (Trie String)

  --- Returns the type of the value.
  typeOf :: a -> RWType

  readListRW :: Trie String -> String -> ([a], String)
  readListRW _    ('0' : cs) = ([], cs)
  readListRW strs ('1' : cs) = (x : xs, r2)
    where
      (x,r1) = readRW strs cs
      (xs,r2) = readListRW strs r1

  showListRW :: RWParameters  -> Trie String -> [a] -> (Trie String,ShowS)
  showListRW _      strs []       = (strs,   showString "0")
  showListRW params strs (x : xs) = (strs'', showString "1" . x' . xs')
    where
      (strs',x') = showRW params strs x
      (strs'',xs') = showListRW params strs' xs

  writeListRW :: RWParameters -> Handle -> [a] -> Trie String -> IO (Trie String)
  writeListRW _      h [] strs = hPutStr h "0" >> return strs
  writeListRW params h (x : xs) strs =
    hPutStr h "1" >> writeRW params h x strs >>= writeListRW params h xs

------------------------------------------------------------------------------
-- `ReadtWrite` instances of prelude types.

instance ReadWrite Int where
  readRW _ cs = (readInt n,r)
    where
      (n,_ : r) = span (flip (/=) ';') cs
      
  showRW _ strs n = (strs, shows n . showString ";")

  writeRW _ h n strs = hPutStr h (show n ++ ";") >> return strs

  typeOf _ = monoRWType "Int"

instance ReadWrite Float where
  readRW _ cs = (read n,r)
    where
      (n,_ : r) = span (flip (/=) ';') cs

  showRW _ strs n = (strs,shows n . showString ";")

  writeRW _ h n strs = hPutStr h (show n ++ ";") >> return strs

  typeOf _ = monoRWType "Float"

instance ReadWrite Char where
  readRW _ (c : cs)
    | c /= '\\' = (c,cs)
    | otherwise
    = case cs of
        '"' : cs1 -> ('"',cs1)
        'a' : cs1 -> ('\a',cs1)
        'b' : cs1 -> ('\b',cs1)
        't' : cs1 -> ('\t',cs1)
        'n' : cs1 -> ('\n',cs1)
        'v' : cs1 -> ('\v',cs1)
        'f' : cs1 -> ('\f',cs1)
        'r' : cs1 -> ('\r',cs1)
        '\\' : cs1 -> ('\\',cs1)
        _ -> error "Invalid escape sequence"

  showRW _ strs c = (strs, showString $ escapeChar c)

  writeRW _ h c strs = do hPutStr h (escapeChar c) >> return strs

  readListRW strs cs =
    case index of
      (_:_) -> (fromJust $ T.lookup index strs, r)
      []    -> readStubString strs r
    where
      (index,r) = readStringId cs :: (String,String)
      readStubString _ cs1 = (str,cs')
        where
          (str,_ : cs') = span (flip (/=) '"') cs1

  showListRW params strs str = (strs',index)
    where
      (strs',index) = writeString params strs str

  writeListRW params h str strs = hPutStr h (index "") >> return strs'
    where
      (strs',index) = writeString params strs str

  typeOf _ = monoRWType "Char"

instance ReadWrite Bool where
  readRW strs ('0' : r0) = (False,r0)
  readRW strs ('1' : r0) = (True,r0)

  showRW params strs0 False = (strs0,showChar '0')
  showRW params strs0 True  = (strs0,showChar '1')

  writeRW params h False strs = hPutChar h '0' >> return strs
  writeRW params h True  strs = hPutChar h '1' >> return strs

  typeOf _ = monoRWType "Bool"

-- `ReadWrite` instance for polymorphic lists.
instance ReadWrite a => ReadWrite [a] where
  readRW = readListRW

  showRW = showListRW

  writeRW = writeListRW

  typeOf n = RWType "[]" [typeOf $ get_a' n]
    where
      get_a' :: [a'] -> a'
      get_a' _ = failed 

--- `ReadWrite` instance for `Either` types.
instance (ReadWrite a,ReadWrite b) => ReadWrite (Either a b) where
  readRW strs ('0' : r0) = (Left a',r1)
    where
      (a',r1) = readRW strs r0
  readRW strs ('1' : r0) = (Right a',r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 (Left a') = (strs1, showChar '0' . show1)
    where
      (strs1,show1) = showRW params strs0 a'
  showRW params strs0 (Right a') = (strs1, showChar '1' . show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h (Left a')  strs = hPutChar h '0' >> writeRW params h a' strs
  writeRW params h (Right a') strs = hPutChar h '1' >> writeRW params h a' strs

  typeOf n = RWType "Either" [typeOf (get_a n),typeOf (get_b n)]
    where
      get_a :: Either a' b' -> a'
      get_a _ = failed
      get_b :: Either a' b' -> b'
      get_b _ = failed

--- `ReadWrite` instance for `Maybe` types.
instance ReadWrite a => ReadWrite (Maybe a) where
  readRW strs ('0' : r0) = (Nothing, r0)
  readRW strs ('1' : r0) = (Just a', r1)
    where
      (a',r1) = readRW strs r0

  showRW params strs0 Nothing   = (strs0, showChar '0')
  showRW params strs0 (Just a') = (strs1, showChar '1' . show1)
    where
      (strs1,show1) = showRW params strs0 a'

  writeRW params h Nothing   strs = hPutChar h '0' >> return strs
  writeRW params h (Just a') strs = hPutChar h '1' >> writeRW params h a' strs

  typeOf n = RWType "Maybe" [typeOf (get_a n)]
    where
      get_a :: Maybe a' -> a'
      get_a _ = failed

--- `ReadWrite` instance for type `Ordering`.
instance ReadWrite Ordering where
  readRW strs ('0' : r0) = (LT, r0)
  readRW strs ('1' : r0) = (EQ, r0)
  readRW strs ('2' : r0) = (GT, r0)

  showRW params strs0 LT = (strs0, showChar '0')
  showRW params strs0 EQ = (strs0, showChar '1')
  showRW params strs0 GT = (strs0, showChar '2')

  writeRW params h LT strs = hPutChar h '0' >> return strs
  writeRW params h EQ strs = hPutChar h '1' >> return strs
  writeRW params h GT strs = hPutChar h '2' >> return strs

  typeOf _ = monoRWType "Ordering"

--- `ReadWrite` instance for unit type.
instance ReadWrite () where
  readRW _ cs = ((), cs)

  showRW _ strs () = (strs, showString "")

  writeRW _ _ () strs = return strs

  typeOf _ = monoRWType "()"

instance (ReadWrite a,ReadWrite b) => ReadWrite (a,b) where
  readRW strs cs = ((x, y), r2)
    where
      (x, r1) = readRW strs cs
      (y, r2) = readRW strs r1

  showRW params strs (x,y) = (strs'',x' . y')
    where
      (strs',  x') = showRW params strs  x
      (strs'', y') = showRW params strs' y

  writeRW params h (x, y) strs = writeRW params h x strs >>= writeRW params h y

  typeOf n = RWType "()" [typeOf $ get_a' n,typeOf $ get_b' n]
    where
      get_a' :: (a', b') -> a'
      get_a' _ = failed 
      get_b' :: (a', b') -> b'
      get_b' _ = failed

instance (ReadWrite a,ReadWrite b,ReadWrite c) => ReadWrite (a,b,c) where
  readRW strs cs = ((x,y,z),r3)
    where
      (x,r1) = readRW strs cs
      (y,r2) = readRW strs r1
      (z,r3) = readRW strs r2

  showRW params strs (x,y,z) = (strs''',x' . (y' . z'))
    where
      (strs',x') = showRW params strs x
      (strs'',y') = showRW params strs' y
      (strs''',z') = showRW params strs'' z

  writeRW params h (x,y,z) strs = 
    (writeRW params h x strs >>= writeRW params h y) >>= writeRW params h z

  typeOf n = RWType "()" [typeOf $ get_a' n,typeOf $ get_b' n,typeOf $ get_c' n]
    where
      get_a' :: (a',b',c') -> a'
      get_a' _ = failed 
      get_b' :: (a',b',c') -> b'
      get_b' _ = failed 
      get_c' :: (a',b',c') -> c'
      get_c' _ = failed

instance (ReadWrite a,ReadWrite b,ReadWrite c,ReadWrite d) => ReadWrite (a, b, c, d) where
  readRW strs cs = ((x,y,z,w),r4)
    where
      (x,r1) = readRW strs cs
      (y,r2) = readRW strs r1
      (z,r3) = readRW strs r2
      (w,r4) = readRW strs r3

  showRW params strs (x,y,z,w) = (strs'''',x' . (y' . (z' . w')))
    where
      (strs',x')    = showRW params strs x
      (strs'',y')   = showRW params strs' y
      (strs''',z')  = showRW params strs'' z
      (strs'''',w') = showRW params strs''' w

  writeRW params h (x,y,z,w) strs =
    writeRW params h x strs >>= writeRW params h y 
                            >>= writeRW params h z 
                            >>= writeRW params h w

  typeOf n = RWType "()" [typeOf $ get_a' n, typeOf $ get_b' n,
                          typeOf $ get_c' n, typeOf $ get_d' n]
    where
      get_a' :: (a', b', c', d') -> a'
      get_a' _ = failed 
      get_b' :: (a', b', c', d') -> b'
      get_b' _ = failed 
      get_c' :: (a', b', c', d') -> c'
      get_c' _ = failed 
      get_d' :: (a', b', c', d') -> d'
      get_d' _ = failed

--- Reads an integer.
readInt :: String -> Int
readInt [] = error "readInt: empty string"
readInt (c:cs) | c == '-'  = -readInt' cs
               | otherwise =  readInt' (c:cs)
 where 
  readInt' = foldl (\n c1 -> n * 10 + (ord c1 - ord '0')) 0

--- Writing RWParameters . 
data RWParameters  = RWParameters 
  { minStrLen   :: Int -- minimum string length for a string to be considered
                       -- a stub (-> not extracted)
  , alphabetLen :: Int -- length of the alphabet
  }

--- Default RWParameters  for writing compact data.
defaultParams :: RWParameters 
defaultParams = RWParameters  6 26

--- Represents a type in the compact data representation.
data RWType = RWType String [RWType]
 deriving Eq

--- Creates a representation of a monomorphic type.
monoRWType :: String -> RWType
monoRWType name = RWType name []

--- Pretty-prints a type.
ppType :: RWType -> String
ppType (RWType name args)
  | args == []                       = name
  | name == "[]" && length args == 1 = "[" ++ (ppType (head args) ++ "]")
  | name == "()"                     = "(" ++ intercalate ", " (map ppType' args) ++ ")"
  | otherwise                        = name ++ " " ++ unwords (map ppType' args)
  where
    ppType' x
      | isMonomorphic x = ppType x
      | bracketed x = ppType x
      | otherwise = "(" ++ (ppType x ++ ")")
    isMonomorphic x =
      case x of
        RWType _ [] -> True
        _ -> False
    bracketed t =
      case t of
        RWType "[]" [_] -> True
        RWType "()" _ -> True
        _ -> False

--- Returns a type at a given position of a type.
typeAt :: [Int] -> RWType -> RWType
typeAt [] x = x
typeAt (i : is) (RWType _ args) = typeAt is (args !! i)

--- The coding.
lookupCoding :: Int -> Char
lookupCoding x = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~" !! x
  
--- Converts an integer (index) to a string (coding).
intToASCII :: Int -> Int -> String
intToASCII lc n
  | n < lc = [lookupCoding n]
  | otherwise = intToASCII lc (div n lc) ++ [lookupCoding (mod n lc)]

--- Shows a string. 
--- 
--- If the string is long, it is extracted and represented only once.
--- Otherwise, it is inlined.
writeString :: RWParameters -> Trie String -> String
            -> (Trie String, String -> String)
writeString (RWParameters  sLen aLen) strs s
  | isStub s = (strs,showChar ';' . (showString s . showChar '"'))
  | otherwise
  = case T.lookup s strs of
      Just i -> (strs,showString i . showChar ';')
      Nothing ->
        let coding = intToASCII aLen (T.size strs)
        in (T.insert s coding strs, showString coding . showChar ';')
  where
    isStub str =
      length str < sLen && not (elem '"' str) && not (containsNewline str)

--- Char escaping 
escapeChar :: Char -> String
escapeChar c =
  case c of
    '"' -> "\\\""
    '\a' -> "\\a"
    '\b' -> "\\b"
    '\t' -> "\\t"
    '\n' -> "\\n"
    '\v' -> "\\v"
    '\f' -> "\\f"
    '\r' -> "\\r"
    '\\' -> "\\\\"
    _ -> [c]

--- Special case: Carriage return
escapeCarriageReturn :: String -> String
escapeCarriageReturn [] = []
escapeCarriageReturn (c : cs)
  | c == '\r' = '\\' : ('r' : escapeCarriageReturn cs)
  | otherwise = c : escapeCarriageReturn cs

--- Special case: Carriage return
unescapeCarriageReturn :: String -> String
unescapeCarriageReturn [] = []
unescapeCarriageReturn (c : cs) =
  case c of
    '\\' ->
      case cs of
        'r' : cs' -> '\r' : unescapeCarriageReturn cs'
        _ -> c : unescapeCarriageReturn cs
    _ -> c : unescapeCarriageReturn cs

--- String output. If the string contains a newline, the length is prepended.
--- Otherwise, a newline is appended as a delimiter.
outputStr :: String -> String
outputStr s
  | containsNewline s = let s' = escapeCarriageReturn s
                        in show (length s') ++ (";" ++ s')
  | otherwise = ";" ++ (s ++ "\n")

--- Checks if the string contains some kind of return character.
containsNewline :: String -> Bool
containsNewline s = elem '\n' s || elem '\r' s

keysOrdByVal :: Trie String -> [String]
keysOrdByVal m = map fst (sortBy ordHex $ T.toList m)
 where
  ordHex :: Ord b => (a,[b]) -> (c,[b]) -> Bool
  ordHex (_,a) (_,b)
    | length a < length b = True
    | length a > length b = False
    | otherwise           = a < b

--- Parses a string id. 
readStringId :: String -> (String,String)
readStringId [] = error "readStringId: empty string"
readStringId (c : cs)
  | c == ';'  = ([],cs)
  | otherwise = let (xs,r1) = readStringId cs in (c : xs,r1)

------------------------------------------------------------------------------
--- Data reading and writing

--- Parses a compact data representation and returns the value.
--- If the parse failes (e.g. due to a type mismatch), `Nothing` is returned.
---
--- This operation might fail if the input is not well-formed.
readData :: ReadWrite a => String -> Maybe a
readData ls =
  let n@(_,t,_,_) = parseInput ls
      result = calc n
  in if ppType (typeOf (fst result)) == t
      then Just $ fst result
      else Nothing
 where
  calc (sLen,_,encoding,strings) =
    readRW (T.fromList $ zip (map (intToASCII sLen) (enumFrom 0)) strings)
           encoding

--- Reads a file containing a compact data representation,
--- parses the contents and returns the value. 
---
--- If the parse failes (e.g. due to a type mismatch or a bad input format),
--- `Nothing` is returned.
readDataFile :: ReadWrite a => FilePath -> IO (Maybe a)
readDataFile file = do
  dt <- readFile file
  catch (return $ readData dt) (\_ -> return Nothing)

--- Writes some data to a file containing a compact data representation.
writeDataFile :: ReadWrite a => FilePath -> a -> IO ()
writeDataFile = writeDataFileP defaultParams

--- Writes some data to a file containing a compact data representation
--- and use specific RWParameters .
writeDataFileP :: ReadWrite a => RWParameters  -> FilePath -> a -> IO ()
writeDataFileP params file x =
  do h <- openFile file WriteMode
     hPutStr h (show (alphabetLen params) ++ "\n")
     hPutStr h (ppType (typeOf x) ++ "\n")
     written <- writeRW params h x T.empty
     hPutStr h "\n"
     let strs = keysOrdByVal written
     mapM_ (hPutStr h) (map outputStr strs)
     hClose h

--- Takes the input and chops it into a parametrized layout "version"
--- (alphabet length), type, an encoding and a list of strings.
parseInput :: String -> (Int, String,String,[String])
parseInput s = (readInt sLen, t, encoding, parseStrings strings)
 where
  (sLen, _ : sx) = break (flip (==) '\n') s
  (t,    _ : s') = break (flip (==) '\n') sx
  (encoding,_ : strings) = break (flip (==) '\n') s'
  parseStrings xs =
    case xs of
      [] -> []
      _  -> let (len,_ : xs') = break (flip (==) ';') xs
            in ifThenElse (Prelude.null len)
                (let (str,xs'') = span (flip (/=) '\n') xs'
                  in str : parseStrings (drop 1 xs''))
                (let (str,xs'') = splitAt (read len :: Int) xs'
               in unescapeCarriageReturn str : parseStrings xs'')

--- Converts a given data value into a compact string representation.
---
--- This is rarely what you want. Use 'writeDataFile' if you want to write
--- the data into a file.
showData :: ReadWrite a => a -> String
showData = showDataP defaultParams

--- Converts data to a compact string representation using specific RWParameters .
---
--- This is rarely what you want. Use writeDataFileP if you want to write
--- the data into a file.
showDataP :: ReadWrite a => RWParameters  -> a -> String
showDataP params x =
  (show (alphabetLen params) ++ "\n") ++
  (ppType (typeOf x) ++ "\n") ++ (l "" ++ "\n") ++
  concatMap outputStr (keysOrdByVal ls)
 where
  (ls,l) = showRW params T.empty x
