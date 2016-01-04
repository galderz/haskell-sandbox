module HaskellBook.Chapter04.Exercises where
  
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if (x < 0) then -x else x
-- myAbs x = if (x < 0) then (negate x) else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

f' :: [a] -> Int
f' xs = w `x` 1
  where w = length xs
  
x' y = y

x'' :: [a] -> a
x'' (x:xs) = x

f'' :: (a, b) -> a
f'' (a, b) = a