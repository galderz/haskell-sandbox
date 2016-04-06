module HaskellBook.Chapter09.Cipher where


import Data.Char


caesar :: String -> Int -> String
caesar "" _ = 
    ""
caesar (x:xs) n = 
    chr (i + chAt) : (caesar xs n)
    where chAt = mod ((ord x) + n - i) 26
          i = ordFst x
          ordFst ch
            | isUpper ch = 65
            | otherwise = 97


unCaesar :: String -> Int -> String
unCaesar s n = 
    caesar s (negate n)
