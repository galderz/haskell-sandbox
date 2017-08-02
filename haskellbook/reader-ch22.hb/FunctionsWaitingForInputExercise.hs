import Control.Applicative
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
tupled =
    do  x <- cap
        y <- rev
        return (x, y)


tupledBasic :: [Char] -> ([Char], [Char])
tupledBasic xs =
    (cap xs, rev xs)


tupledLift :: [Char] -> ([Char], [Char])
tupledLift =
    liftA2 ((,)) cap rev


main :: IO ()
main =
    do  print $ composed "Julie"
        print $ fmapped "Chris"
        print $ tupled "Julie"
        print $ tupledBasic "Julie"
        print $ tupledLift "Julie"
