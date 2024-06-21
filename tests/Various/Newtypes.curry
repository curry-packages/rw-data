{-
  Tests data and newtype declarations as well as type synonyms.
-}

module Various.Newtypes where

data MyTypeOk = MyCons

newtype MyNewType = MyCons2 Int

type MyInt = Int

newtype MySecondNewType = MyCons3 MyNewType