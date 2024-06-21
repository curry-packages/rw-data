module TypeIO.Defs where

data MyDef1 = MyCons1 Int Int | MyCons2 Char
  deriving (Read, Show, Eq)

data MyMaybe a = MyJust a | MyNothing
  deriving (Read, Show, Eq)

data MyProblemA a = MyProblemA (MyProblemB a)
  deriving (Read, Show, Eq)

data MyProblemB a = MyProblemB (MyProblemA a) | Ok (MyMaybe a)
  deriving (Read, Show, Eq)

data ThreeCycleA a = ThreeCycleA (ThreeCycleB a)
  deriving (Read, Show, Eq)

data ThreeCycleB a = ThreeCycleB (ThreeCycleC a) | Yay (Escape a)
  deriving (Read, Show, Eq)

data ThreeCycleC a = ThreeCycleC (ThreeCycleA a) | Good (MyMaybe (MyMaybe a))
  deriving (Read, Show, Eq)

data Escape a = VeryGood a
  deriving (Read, Show, Eq)

data Lots a b c d e f g = Nah 
  deriving (Read, Show, Eq)