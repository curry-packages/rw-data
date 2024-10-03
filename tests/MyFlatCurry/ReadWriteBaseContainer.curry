--- Modified: added parametrized containers for string mapping (reading)
--- 
--- Implements the ReadWrite class, which is used to read and write data to files. Also contains predefined instances for some types.
---
--- @author Lasse Züngel

module MyFlatCurry.ReadWriteBaseContainer where

import Data.Maybe
import Data.List
import Data.Map
import System.IO
import Text.Show

import Prelude hiding (ShowS, showString, showChar, shows)

import Data.Trie as T

import Debug.Trace

import Control.Search.Unsafe (oneValue)

class ReadWrite a where
  readRW :: Container c => c -> String -> (a,String)

  showRW :: RWParameters  -> T.Trie String -> a -> (T.Trie String, Text.Show.ShowS)

  writeRW :: RWParameters  -> System.IO.Handle -> a -> T.Trie String -> IO (T.Trie String)

  -- Returns the type of the value.
  typeOf :: a -> RWType

  readListRW :: Container c => c -> String -> ([a],String)
  readListRW strs ('0' : cs) = ([],cs)
  readListRW strs ('1' : cs) = (x : xs,r2)
    where
      (x,r1) = readRW strs cs
      (xs,r2) = readListRW strs r1

  showListRW :: RWParameters  -> T.Trie String -> [a] -> (T.Trie String,ShowS)
  showListRW _      strs [] = (strs,showString "0")
  showListRW params strs (x : xs) = (strs'',showString "1" . x' . xs')
    where
      (strs',x') = showRW params strs x
      (strs'',xs') = showListRW params strs' xs

  writeListRW :: RWParameters  -> System.IO.Handle -> [a] -> T.Trie String -> IO (T.Trie String)
  writeListRW _      h [] strs = System.IO.hPutStr h "0" >> return strs
  writeListRW params h (x : xs) strs =
    System.IO.hPutStr h "1" >> writeRW params h x strs >>= writeListRW params h xs

instance ReadWrite Int where
  readRW _ cs = (readInt n,r)
    where
      (n,_ : r) = span (flip (/=) ';') cs
      
  showRW _ strs n = (strs,shows n . showString ";")

  writeRW _ h n strs = do System.IO.hPutStr h (show n ++ ";") >> return strs

  typeOf _ = monoRWType "Int"

instance ReadWrite Float where
  readRW _ cs = (read n,r)
    where
      (n,_ : r) = span (flip (/=) ';') cs

  showRW _ strs n = (strs,shows n . showString ";")

  writeRW _ h n strs = do System.IO.hPutStr h (show n ++ ";") >> return strs

  typeOf _ = monoRWType "Float"

instance ReadWrite a => ReadWrite [a] where
  readRW = readListRW

  showRW = showListRW

  writeRW = writeListRW

  typeOf n = RWType "[]" [typeOf $ get_a' n]
    where
      get_a' :: [a'] -> a'
      get_a' _ = failed 

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

  showRW _ strs c = (strs,showString $ escapeChar c)

  writeRW _ h c strs = do System.IO.hPutStr h (escapeChar c) >> return strs

  readListRW strs cs =
    case index of
      [] -> readStubString r
      _  -> (Data.Maybe.fromJust $ contLookup index strs,r)
    where
      (index,r) = readStringId strs cs :: (String,String)
      readStubString cs = (str,cs')
        where
          (str,_ : cs') = span (flip (/=) '"') cs

  showListRW params strs str = (strs',index)
    where
      (strs',index) = writeString params strs str

  writeListRW params h str strs = System.IO.hPutStr h (index "") >> return strs'
    where
      (strs',index) = writeString params strs str

  typeOf _ = monoRWType "Char"

instance ReadWrite () where
  readRW _ cs = ((),cs)

  showRW _ strs () = (strs,showString "")

  writeRW _ h () strs = return strs

  typeOf _ = monoRWType "()"

instance (ReadWrite a,ReadWrite b) => ReadWrite (a,b) where
  readRW strs cs = ((x,y),r2)
    where
      (x,r1) = readRW strs cs
      (y,r2) = readRW strs r1

  showRW params strs (x,y) = (strs'',x' . y')
    where
      (strs',x') = showRW params strs x
      (strs'',y') = showRW params strs' y

  writeRW params h (x,y) strs = writeRW params h x strs >>= writeRW params h y

  typeOf n = RWType "()" [typeOf $ get_a' n,typeOf $ get_b' n]
    where
      get_a' :: (a',b') -> a'
      get_a' _ = failed 
      get_b' :: (a',b') -> b'
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

  writeRW params h (x,y,z) strs = (writeRW params h x strs >>= writeRW params h y) >>= writeRW params h z

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
    writeRW params h x strs >>= writeRW params h y >>= writeRW params h z >>= writeRW params h w

  typeOf n = RWType "()" [typeOf $ get_a' n,typeOf $ get_b' n,typeOf $ get_c' n,typeOf $ get_d' n]
    where
      get_a' :: (a',b',c',d') -> a'
      get_a' _ = failed 
      get_b' :: (a',b',c',d') -> b'
      get_b' _ = failed 
      get_c' :: (a',b',c',d') -> c'
      get_c' _ = failed 
      get_d' :: (a',b',c',d') -> d'
      get_d' _ = failed

----------------------------------------------------------------------------------------------------
--- abstract string container

-- Wrappers for different string containers (for reading).
-- 
-- Wrapping the containers and using the Container class as an abstraction yields a 3% runtime slowdown,
-- but it allows us to easily switch between different containers which is useful for testing and bench-
-- marking. In production, one should usually use 'Trie String' directly instead of resorting to this
-- abstraction.

newtype ListString = ListString [(String,String)]
idListString = 0

newtype MapString  = MapString (Map String String)
idMapString = 1

newtype TrieString = TrieString (Trie String)
idTrieString = 2

--- Abstract string container
class Container a where
  contEmpty    :: a
  contInsert   :: String -> String -> a -> a
  contLookup   :: String -> a -> Maybe String
  contSize     :: a -> Int
  contFromList :: [(String,String)] -> a
  contToList   :: a -> [(String,String)]

instance Container ListString where
  contEmpty                     = ListString []
  contInsert k v (ListString m) = ListString ((k,v) : m)
  contLookup k (ListString m)   = Prelude.lookup k m
  contSize (ListString m)       = Prelude.length m
  contFromList xs               = ListString xs
  contToList (ListString m)     = m

instance Container MapString where
  contEmpty                     = MapString Data.Map.empty
  contInsert k v (MapString m)  = MapString (Data.Map.insert k v m)
  contLookup k (MapString m)    = Data.Map.lookup k m
  contSize (MapString m)        = Data.Map.size m
  contFromList xs               = MapString (Data.Map.fromList xs)
  contToList (MapString m)      = Data.Map.toList m

instance Container TrieString where
  contEmpty                     = TrieString T.empty
  contInsert k v (TrieString m) = TrieString (T.insert k v m)
  contLookup k (TrieString m)   = T.lookup k m
  contSize (TrieString m)       = T.size m
  contFromList xs               = TrieString (T.fromList xs)
  contToList (TrieString m)     = T.toList m

-- Reads an integer.
readInt :: String -> Int
readInt (c:cs) | c == '-' = -readInt' cs
               | otherwise = readInt' (c:cs)
readInt' cs = foldl (\n c -> n * 10 + (ord c - ord '0')) 0 cs

data RWParameters  = RWParameters 
  { minStrLen   :: Int -- Minimum string length for a string to be considered a stub (-> not extracted)
  , alphabetLen :: Int -- Length of the alphabet
  , container   :: Int -- id of the container to use. 0 = ListString, 1 = MapString, 2 = TrieString
  }

-- Default RWParameters 
defaultParams :: RWParameters 
defaultParams = RWParameters  6 26 idTrieString

-- Represents a type 
data RWType = RWType String [RWType]
 deriving Eq

-- Creates a monomorphic type
monoRWType :: String -> RWType
monoRWType name = RWType name []

-- Pretty-prints a type
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

-- Returns a type at a given position of a type.
typeAt :: [Int] -> RWType -> RWType
typeAt [] x = x
typeAt (i : is) (RWType _ args) = typeAt is (args !! i)

-- The coding
lookupCoding :: Int -> Char
lookupCoding x = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~" !! x
  
-- Converts an integer (index) to a string (coding).
intToASCII :: Int -> Int -> String
intToASCII lc n
  | n < lc = [lookupCoding n]
  | otherwise = intToASCII lc (div n lc) ++ [lookupCoding (mod n lc)]

-- Shows a string. 
-- 
-- If the string is long, it is extracted and represented only once. Otherwise, it is inlined.
writeString :: RWParameters  -> T.Trie String -> String -> (T.Trie String, String -> String)
writeString (RWParameters  sLen aLen _) strs s
  | isStub s = (strs,showChar ';' . (showString s . showChar '"'))
  | otherwise
  = case T.lookup s strs of
      Just i -> (strs,showString i . showChar ';')
      Nothing ->
        let coding = intToASCII aLen (T.size strs)
        in (T.insert s coding strs,showString coding . showChar ';')
  where
    isStub str =
      lengthBelow str sLen && ((not $ elem '"' str) && (not $ containsNewline str))
      where
        lengthBelow []       n = n > 0
        lengthBelow (c : cs) n = lengthBelow cs (n - 1)

-- Char escaping 
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

-- Special case: Carriage return
escapeCarriageReturn :: String -> String
escapeCarriageReturn [] = []
escapeCarriageReturn (c : cs)
  | c == '\r' = '\\' : ('r' : escapeCarriageReturn cs)
  | otherwise = c : escapeCarriageReturn cs

-- Special case: Carriage return
unescapeCarriageReturn :: String -> String
unescapeCarriageReturn [] = []
unescapeCarriageReturn (c : cs) =
  case c of
    '\\' ->
      case cs of
        'r' : cs' -> '\r' : unescapeCarriageReturn cs'
        _ -> c : unescapeCarriageReturn cs
    _ -> c : unescapeCarriageReturn cs

-- Parses some input and returns the value. If the parse failed (e.g. due to a type mismatch), Nothing is returned.
readData :: ReadWrite a => String -> Maybe a
readData ls =
  let n@(sLen,_,t,_,_) = parseInput ls
      result = calc n
  in if ppType (typeOf (fst result)) == t
      then Just $ fst result
      else Nothing
 where
  calc (sLen,containerId,_,encoding,strings) 
    = case containerId of
        0 -> readRW (contFromList $ zip (map (intToASCII sLen) (enumFrom 0)) strings :: ListString) encoding
        1 -> readRW (contFromList $ zip (map (intToASCII sLen) (enumFrom 0)) strings :: MapString)  encoding
        2 -> readRW (contFromList $ zip (map (intToASCII sLen) (enumFrom 0)) strings :: TrieString) encoding

-- Writes some data to a file. 
writeDataFile :: ReadWrite a => String -> a -> IO ()
writeDataFile = writeDataFileP defaultParams

writeDataFileP :: ReadWrite a => RWParameters  -> String -> a -> IO ()
writeDataFileP params file x =
  do h <- System.IO.openFile file System.IO.WriteMode
     System.IO.hPutStr h (show (alphabetLen params) ++ " " ++ (show (container params)) ++ "\n")
     System.IO.hPutStr h (ppType (typeOf x) ++ "\n")
     written <- T.toList <$> writeRW params h x (T.empty)
     System.IO.hPutStr h "\n"
     let strs = keysOrdByVal written
     mapM_ (System.IO.hPutStr h) (map outputStr strs)
     System.IO.hClose h

-- Takes the input and chops it into a parametrized layout "version" (alphabet length), the reading container, type, an encoding and a list of strings.
parseInput :: String -> (Int, Int, String, String, [String])
parseInput s = (readInt sLen, readInt container, t, encoding, parseStrings strings)
  where
    (lenAndContainer, _ : sx) = break (flip (==) '\n') s
    sLen = head $ words lenAndContainer
    container = head $ tail $ words lenAndContainer
    (t,    _ : s') = break (flip (==) '\n') sx
    (encoding,_ : strings) = break (flip (==) '\n') s'
    parseStrings xs =
      case xs of
        [] -> []
        _ ->
          let (len,_ : xs') = break (flip (==) ';') xs
          in ifThenElse (Prelude.null len)
              (let (str,xs'') = span (flip (/=) '\n') xs'
               in str : parseStrings (drop 1 xs''))
              (let (str,xs'') = splitAt (read len :: Int) xs'
               in unescapeCarriageReturn str : parseStrings xs'')

-- Converts data to a string. 
--
-- This is rarely what you want. Use showData if you want to write the data to a file.
showData :: ReadWrite a => a -> String
showData = showDataP defaultParams

showDataP :: ReadWrite a => RWParameters  -> a -> String
showDataP params x = (show (alphabetLen params) ++ " " ++ show (container params) ++ "\n") ++ (ppType (typeOf x) ++ "\n") ++ (l ++ "\n") ++ concatMap outputStr (keysOrdByVal ls)
  where
    (ls,l) = let (ls', l') = showRW params (T.empty) x
             in (T.toList ls', l' "")

outputStr :: String -> String
outputStr s
  | containsNewline s = let s' = escapeCarriageReturn s in show (length s') ++ (";" ++ s')
  | otherwise = ";" ++ (s ++ "\n")

-- Checks if the string contains some kind of return character.
containsNewline :: String -> Bool
containsNewline s = elem '\n' s || elem '\r' s

ordHex :: Ord b => (a,[b]) -> (c,[b]) -> Bool
ordHex (_,a) (_,b)
  | length a < length b = True
  | length a > length b = False
  | otherwise = a < b

keysOrdByVal :: [(String, String)] -> [String]
keysOrdByVal m = map fst (Data.List.sortBy ordHex m)

readStringId :: Container c => c -> String -> (String,String)
readStringId strs (c : cs)
  | c == ';' = ([],cs)
  | otherwise = let (xs,r1) = readStringId strs cs in (c : xs,r1)