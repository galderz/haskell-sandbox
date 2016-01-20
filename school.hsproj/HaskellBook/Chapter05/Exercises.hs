module HaskellBook.Chapter05.Exercises where
  
-- How might that change when we apply
-- it to the following value?
-- R: It's made more explicit to String
myConcat x = x ++ "yo"

-- Applied to a value
-- R: Made more explicit, to Fractional
myMult x = (x / 3) * 5

-- R: Made more explicit, to [Char]
myTake x = take x "hey you"

-- R: Made more explicit, to Int because of length
myCom x = x > (length [1..10])

-- R: Made more explicit, to Char
myAlph x = x < 'z'

bigNum = (^) 5 $ 10
--wahoo = bigNum $ 10

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, a) -> a
functionS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r (x:xs) = xs
-- r x = []
-- r x = x

-- compose function
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = \x -> f $ g x

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
     where x = "Singin"
           y = "Somewhere"
           
main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1
  

