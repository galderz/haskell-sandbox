module HaskellBook.Chapter03.Strings where

-- Print1
main :: IO ()
main = putStrLn "kaixo mundua"

-- Print2
main' :: IO ()
main' = do
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"
  
main'' :: IO ()
main'' = do
  putStr "Count to four for me:"
  putStrLn   "one, two"
  putStrLn   ", three, and"
  putStr " four!"
  
-- Print3
myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

myGreetingMain :: IO ()
myGreetingMain = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = concat [hello, " ", world]

-- GlobalLocal: example of global vs local variables
topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5

area d = pi * (r * r)
  where r = d / 2

-- Print3Flipped
myGreeting' :: String
myGreeting' = (++) "hello" " world!"

hello' :: String
hello' = "hello"

world' :: String
world' = "world!"

myGreetingMain' :: IO ()
myGreetingMain' = do
    putStrLn myGreeting'
    putStrLn secondGreeting
    where secondGreeting = (++) hello' ((++) " " world')

-- Print4Broken
greeting :: String
greeting = "Yarrrrr" 

printSecond :: IO ()
printSecond = do
   putStrLn greeting
   
mainPrint4 :: IO ()
mainPrint4 = do
  putStrLn greeting
  printSecond
