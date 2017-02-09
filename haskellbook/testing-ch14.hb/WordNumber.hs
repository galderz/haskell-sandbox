module WordNumber where

import Data.List (intersperse)


digitToWord :: Int -> String
digitToWord 0 =
    "zero"
digitToWord 1 =
    "one"
digitToWord 2 =
    "two"
digitToWord 3 =
    "three"
digitToWord 4 =
    "four"
digitToWord 5 =
    "five"
digitToWord 6 =
    "six"
digitToWord 7 =
    "seven"
digitToWord 8 =
    "eight"
digitToWord _ =
    "nine"


digits :: Int -> [Int]
digits n =
    reverse $ go n
    where go n
            | n < 10 =
                n:[]
              | otherwise =
                  (mod n 10) : (go (div n 10))


wordNumber :: Int -> String
wordNumber n =
    concat $ intersperse "-" $ words
    where words =
            map digitToWord (digits n)
