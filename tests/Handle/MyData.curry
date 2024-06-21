module Handle.MyData where

data MyData a = MyCons1 a | MyCons2 (MyData a) (MyData a)
 deriving (Eq, Read, Show)

data MyElem = A | B | C
  deriving (Eq, Read, Show)

-- generate a tree of depth n
genTree :: Int -> MyData MyElem
genTree x | x > 0     = MyCons2 (genTree (x-1)) (genTree (x-1))
          | otherwise = MyCons1 A

data MyBoring = MyOnlyConstructor Int Int Int Int
  deriving (Eq, Read, Show)