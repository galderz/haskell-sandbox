module HaskellBook.Chapter04.BasicTypes where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhh."
  where cool = coolness == "downright frosty yo"

-- with cool as a function rather than a value	
greetIfCool' :: String -> IO ()
greetIfCool' coolness =
  if cool coolness then 
    putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhh."
  where cool v = v == "downright frosty yo"
