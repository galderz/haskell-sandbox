module HaskellBook.Chapter09.NonStrict where


-- mySum will force both the spine and the values
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

