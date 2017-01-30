module HaskellBook.Chapter13.Vigenere where
  

import Data.Char
  

vigenere :: String -> String -> String
vigenere "" _ = 
  ""
vigenere phr kw =
  couple phr kw kw  


couple :: String -> String -> String -> [Char]
couple "" _ w =
  []
couple msg "" w = 
  couple msg w w
couple (' ':xs) ks w =
  ' ' : couple xs ks w
couple (x:xs) (k:ks) w =
   ch : couple xs ks w
   where ch = rightShift x (ordy)
         ordx = ord x
         ordy = (ord k) - 65


rightShift :: Char -> Int -> Char
rightShift x n =
  chr (i + chAt)
    where chAt = mod ((ord x) + n - i) 26
          i = ordFst x
          ordFst ch
            | isUpper ch = 65
            | otherwise = 97


testSpace :: Char -> Int
testSpace ' ' = 5
testSpace x = 23


main :: IO ()
main = do
  putStrLn "Enter cipher word: "
  word <- getLine
  putStrLn "Enter keyword: "
  keyW <- getLine
  putStrLn ("Vigenere chiper is: " ++ vigenere word keyW) 
  