module HaskellBook.Chapter05.Types where
  
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

funcIgnoreArgs :: a -> a -> a -> String
funcIgnoreArgs x y z = "Blah"

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested :: Integer -> Bool -> Integer
anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)

f :: a -> a -> a -> a
f x y z = x

g :: a -> b -> c -> b
g x y z = y

h :: (Num a, Num b) => a -> b -> b
h x y = y

jackal :: (Ord a, Eq b) => a -> b -> a
jackal x y = x

kessel :: (Ord a, Num b) => a -> b -> a
kessel x y = x

id' :: a -> a
id' x = x

monoid :: a -> a -> a
monoid x y = x

monoid' :: a -> a -> a
monoid' x y = y

ignoreFirst :: a -> b -> b
ignoreFirst x y = y

f' :: Num a => a -> a -> a
f' x y = x + y + 3

f'' x y = x + y + 3
