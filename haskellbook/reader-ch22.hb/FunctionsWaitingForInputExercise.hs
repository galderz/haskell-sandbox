import Data.Char


cap :: [Char] -> [Char]
cap =
    map toUpper


rev :: [Char] -> [Char]
rev =
    reverse


composed :: [Char] -> [Char]
composed =
    undefined


fmapped :: [Char] -> [Char]
fmapped =
    undefined


main :: IO ()
main =
    do  print $ composed "Julie"
