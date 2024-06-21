module Peano.Peano where

--- Representation of natural numbers using Peano numbers
data Nat = Zero | Successor Nat
  deriving (Show, Eq, Read)

--- Int to Nat
toNat :: Int -> Nat
toNat n | n < 0     = error "toNat: negative number"
        | n == 0    = Zero
        | otherwise = Successor (toNat (n - 1))

--- Nat to Int
fromNat :: Nat -> Int
fromNat Zero          = 0
fromNat (Successor n) = 1 + fromNat n