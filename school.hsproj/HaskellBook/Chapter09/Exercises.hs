module HaskellBook.Chapter09.Exercises where
  

import Data.Char


filterUpper :: String -> String
filterUpper s = 
    filter isUpper s


upperFirst :: String -> String
upperFirst [] = 
    ""
upperFirst (x:xs) =
    toUpper x : xs


upperAll :: String -> String
upperAll [] = 
    ""
upperAll (x:xs) = 
    toUpper x : upperAll xs


upperFirstChar :: String -> Char
upperFirstChar [] = 
    ' '
upperFirstChar s = 
    toUpper $ head s


upperFirstCharDot :: String -> Char
upperFirstCharDot [] = 
    ' '
upperFirstCharDot s = 
    (toUpper . head) s
    

upperFirstCharPF :: String -> Char
upperFirstCharPF = (toUpper . head)
