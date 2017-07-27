import Control.Applicative


boop = (*2)


doop = (+10)


bip :: Integer -> Integer
bip =
    boop . doop


-- Another way to write bip
bloop :: Integer -> Integer
bloop =
    -- (*2) ((+10) x)
    fmap boop doop


bloop' :: Integer -> Integer
bloop' x =
    (*2) ((+10) x)


-- Similar to above, but argument will be passed to both
-- boop and doop in paralell and the results added together
bbop :: Integer -> Integer
bbop =
    (+) <$> boop <*> doop


-- Equivalent to bbop
duwop :: Integer -> Integer
duwop =
    liftA2 (+) boop doop


main :: IO ()
main =
    do  print $ bip 5 -- 30
        print $ bloop 5 -- 30
        print $ bloop' 5 -- 30
        --
        print $ bbop 5 -- 25
        print $ ((+) <$> (*2) <*> (+10)) 5
        -- it's just function composition
        print $ ((+) . (*2)) 10 5
        print $ ((+) <$> (*2)) 10 5
