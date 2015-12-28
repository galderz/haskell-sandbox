module HaskellBook.Chapter02.Hello where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3
half x = x / 2
square x = x * x

sqPi x = 3.14 * (square x)

foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

printInc n = print plusTwo
  where plusTwo = n + 2
  
printInc2 n = let plusTwo = n + 2
                in print plusTwo

-- let x = 5; y = 6 in x * y
mult1     = x * y 
  where x = 5
        y = 6

