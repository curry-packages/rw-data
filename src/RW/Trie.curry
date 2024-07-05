------------------------------------------------------------------------------
--- A simple trie data structure.
--- 
--- This implementation assumes that the keys are small. It is not (yet)
--- optimized for arbitrary keys. 
--- That is, no optimizations are done in regards to memory usage
--- (or access speed) when using different keys with unique prefixes.
---
--- Consider a Trie containing the keys a,b,c,aa,ab,ac:
---          root 
---         / | \
---        /  |  \
---       a   b   c
---     / | \    
---    a  b  c
---
--- This is fine. Now consider a Trie containing the keys a,b,Helloworld
---          root
---         / | \
---        /  |  \
---       a   b   H  
---               |
---               e
---               |
---               l
---               |
---               ...
--- This is an issue because lots of nodes without associated values are created
--- (only the leaf nodes a,b,d have values). 
---
--- @author Lasse Züngel
--- @version July 2024
------------------------------------------------------------------------------

module RW.Trie 
  (
  --- * Data types
    Trie(), 
    
  --- * Basic functions 
    empty, null, singleton, size,

  --- * Traversing functions
    insert, lookup, 
    
  --- * List conversion functions
    fromList, toList) where

import Data.List
import Data.Maybe

import qualified Data.Map

--- The size of a trie
type Size = Int

--- Trie data structure
data Trie a = Trie Size (Maybe a) [(Char, Trie a)]
  deriving (Show, Eq)

--- An empty trie.
empty :: Trie a
empty = Trie 0 Nothing []

--- Is the trie empty?
null :: Trie a -> Bool
null x = case x of 
  Trie _ Nothing [] -> True
  _                 -> False

--- Returns the size of the trie.
size :: Trie a -> Int
size (Trie s _ _) = s

--- A singleton trie.
singleton :: Eq a => String -> a -> Trie a
singleton str v = insert str v empty

--- Inserts a value into the trie.
insert :: Prelude.Eq a => String -> a -> Trie a -> Trie a
insert []     v (Trie s _ ts) = Trie (s+1) (Just v) ts
insert (c:cs) v (Trie s v' ts) = case Prelude.lookup c ts of
  Nothing -> Trie (s+1) v' ((c, insert cs v empty) : ts)
  Just t  -> Trie (s+1) v' ((c, insert cs v t) : (filter (\(c', _) -> c' /= c) ts))

--- Looks up a value in the trie.
lookup :: String -> Trie a -> Maybe a
lookup []     (Trie _ v _)  = v
lookup (c:cs) (Trie _ _ ts) = case Prelude.lookup c ts of
  Nothing -> Nothing
  Just t  -> RW.Trie.lookup cs t

--- Converts a list of key-value pairs into a trie.
fromList :: Eq a =>  [(String, a)] -> Trie a
fromList = foldr (uncurry insert) empty

--- Converts a trie into a list of key-value pairs.
toList :: Trie a -> [(String, a)]
toList (Trie _ v ts) = case v of
  Nothing -> concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList t)) ts
  Just z  -> ("", z) :
             concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList t)) ts

-------- tests

alphabet :: [Char]
alphabet = ['a'..'z']

sizeAlphabet :: Int
sizeAlphabet = 26

toKey :: Int -> String
toKey n | n < 0            = error "toKey: negative number"
        | n < sizeAlphabet = [alphabet !! n]
        | otherwise        = toKey (n `div` sizeAlphabet) ++
                             toKey (n `mod` sizeAlphabet)

{-
-- Tests:
keys = map toKey [0..1000]

input = zip keys [0..]

myList = fromList input

myMap = Data.Map.fromList input

lT :: Trie Int -> String -> Int
lT list k = fromJust $ RW.Trie.lookup k list

lP :: [(String, Int)] -> String -> Int
lP list k = fromJust $ Prelude.lookup k list

lMap :: Data.Map.Map String Int -> String -> Int
lMap list k = fromJust $ Data.Map.lookup k list

-- 487ms
sumAllT :: Trie Int -> Int
sumAllT list = sum $ map (lT list) keys

-- 1700ms
sumAllP :: [(String, Int)] -> Int
sumAllP list = sum $ map (lP list) keys

-- 930ms
sumAllMap :: Data.Map.Map String Int -> Int
sumAllMap list = sum $ map (lMap list) keys

tt = sumAllT myList  --  150ms (kics2, n=10000) -  520ms (pakcs, n=1000)
tp = sumAllP input   -- 3150ms (kics2         ) - 1750ms (pakcs        )
tm = sumAllMap myMap --  280ms (kics2         ) -  930ms (pakcs        )
-}
