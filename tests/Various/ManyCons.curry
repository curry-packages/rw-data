module Various.ManyCons where

data MyData a a' = MyData a a'
  deriving (Show, Read, Eq)

data MyFew = FA | FB | FC | FD | FE | FF | FG | FH | FI | FJ | FK | FL | FM | FN | FO | FP 
  deriving (Show, Read, Eq)

data MyMany = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q 
  deriving (Show, Read, Eq)