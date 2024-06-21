{-
  Tests the tool. Expected output: 

  <...>
  Warning: The following data definitions contain function types: "MyData"
           Functions cannot be read or written.
-}

module Various.Functions where

data MyData = MyFunctionWrapper (Int -> Int -> Int)

apply' :: MyData -> Int -> Int -> Int
apply' (MyFunctionWrapper f) = f 

myMinus = MyFunctionWrapper (-)
myPlus = MyFunctionWrapper (+)