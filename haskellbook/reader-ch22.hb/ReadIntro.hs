import Control.Applicative


boop = (*2)


doop = (+10)


bip :: Integer -> Integer
bip =
    boop . doop


-- Another way to write bip
bloop :: Integer -> Integer
bloop =
    fmap boop doop


bloop' :: Integer -> Integer
bloop' x =
    (*2) ((+10) x)


main :: IO ()
main =
    do  print $ bip 5
        print $ bloop 5
        print $ bloop' 5
