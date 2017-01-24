module HaskellBook.Chapter08.Recursion where

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
-- different syntax but same thing
three' = (inc . inc . inc) $ 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times -1) n)

-- abstract recursion out of incTimes:
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

-- make composition more obvious
applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes (n - 1) f $ b

f :: Bool -> Int
f True = error "blah"
f False = 0

f' :: Bool -> Int
f' False = 0

f'' :: Bool -> Maybe Int
f'' False = Just 0
f'' True = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy' :: Numerator -> Denominator -> Quotient
dividedBy' = div

-- Indentation position important!
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)
