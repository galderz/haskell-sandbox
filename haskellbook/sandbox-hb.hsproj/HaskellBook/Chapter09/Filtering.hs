module HaskellBook.Chapter09.Filtering where

-- Write a filter function that would give us all the multiples of 3 out of a list from 1-30
multiplesOf :: Integral a => a -> [a] -> [a]
multiplesOf n xs =
    filter (\x -> (rem x n) == 0) xs


myFilter :: String -> [String]
myFilter s = 
    filter (\x -> x /= "the" && x /= "a" && x /= "an") ws
    where ws = words s
