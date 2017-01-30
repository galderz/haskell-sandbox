module HaskellBook.Chapter09.Transform where


itIsMystery :: String -> [Bool]
itIsMystery xs = 
    map (\x -> elem x "aeiou") xs

