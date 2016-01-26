module HaskellBook.Chapter02.Exercises where

-- Order does not matter in source file
z = 7

x = y ^ 2

waxOn = x * 5

y = z + 8

-- waxOn rewritten with where clause
waxOn'' = p
  where p = x * 5
  
triple a = a * 3

waxOff x = triple x

waxOff' x = 2 ^ triple x

waxOff'' x = (triple x) `div` 10
