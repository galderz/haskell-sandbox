module HaskellBook.Chapter07.Intermission where
  
mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

mTh'' x = \y -> \z -> x * y * z

mTh''' = \x -> \y -> \z -> x * y * z

addOne x = x + 1

addOne' = \x -> x + 1

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1
  
addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

addFive'' = \x y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x

mflip' f x y = f y x

myF :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
myF (a, b, c) (d, e, f) = ((a, d), (c, f))

functionC x y = if (x > y) then x else y

functionC' x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

avgGrade' :: (Fractional a, Ord a) => a -> Char
avgGrade' x
  | otherwise = 'F'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0/59  = 'F'
  where y = x / 100
  
avgGrade'' :: (Fractional a, Ord a) => a -> Char
avgGrade'' x
  | y >= 0.7  = 'C'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.59 = 'D'
  | y < 0/59  = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True 
  | otherwise = False

numbers x
  | x < 0  = (-1)
  | x == 0 = 0
  | x > 0  = 1
  
  
