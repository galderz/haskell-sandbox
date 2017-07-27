import Control.Applicative


boop = (*2)


doop = (+10)


bip :: Integer -> Integer
bip =
    boop . doop


main :: IO ()
main =
    do  print $ bip 5
