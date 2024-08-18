module Trie.Tests where

import RW.Trie
import Test.Prop
import Data.List ( sort )

--- Tests the size of an empty trie.
testEmptySize :: Prop
testEmptySize = RW.Trie.null RW.Trie.empty -=- True

--- Tests the size of a singleton trie.
testSingletonSize :: Prop
testSingletonSize = size (singleton "a" 42) -=- 1

--- Tests `fromList` and `toList` functions.
testConversion :: Prop
testConversion = (sort . toList . fromList) content -=- sort content
 where
  content = [("a", 42), ("b", 43), ("ab", 44)]