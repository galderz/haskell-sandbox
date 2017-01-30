module HaskellBook.Chapter05.TypeKwonDo where

-- Exercise 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f
--h n = g (f n)

-- Exercise 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- Exercise 3
data X
data Y
data Z

xs :: X -> Z
xs = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xs x, yz y)

-- Exercise 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = fst $ ywz (xy x)
