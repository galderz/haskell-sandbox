module HaskellBook.Chapter09.EnumFromTo where

eftChar :: Char -> Char -> [Char]
eftChar x y = reverse (rec x y)
  where rec x y
          | x > y = []
          | x == y = x : []
          | x < y = y : rec x (pred y)


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = reverse (rec x y)
  where rec x y
          | x > y = []
          | x == y = x : []
          | x < y = y : rec x (pred y)


eftInt :: Int -> Int -> [Int]
eftInt x y = reverse (rec x y)
  where rec x y
          | x > y = []
          | x == y = x : []
          | x < y = y : rec x (y - 1)


eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool _ _ = []


--eftOrd' :: Ord a => a -> a -> [a]
--eftOrd' x y = reverse (rec x y)
--  where rec x y
--          | x > y = []
--          | x == y = x : []
--          | x < y = y : rec x (pred y)
