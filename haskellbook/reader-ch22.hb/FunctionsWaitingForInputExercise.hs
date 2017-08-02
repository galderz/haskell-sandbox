import Data.Char


cap :: [Char] -> [Char]
cap =
    map toUpper


rev :: [Char] -> [Char]
rev =
    reverse


composed :: [Char] -> [Char]
composed =
    rev . cap


fmapped :: [Char] -> [Char]
fmapped =
    fmap rev cap


tupled :: [Char] -> ([Char], [Char])
tupled xs =
    (cap xs, rev xs)


main :: IO ()
main =
    do  print $ composed "Julie"
        print $ fmapped "Chris"
        print $ tupled "Julie"
