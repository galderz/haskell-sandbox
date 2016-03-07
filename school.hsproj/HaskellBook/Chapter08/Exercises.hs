module HaskellBook.Chapter08.Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ "mrow" ++ y
-- fill in the types
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

sumAll :: (Eq a, Num a) => a -> a
sumAll 1 = 1
sumAll n = n + sumAll (n - 1)

multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum n 1 = n
multiplyBySum n x = n + (multiplyBySum n (x - 1))
