import Data.Monoid


data Identity a =
    Identity a


-- The point of foldable for Identity is less to reduce
-- the values inside the structure to one value, and more
-- to consume or use the value.
instance Foldable Identity where
    foldr f z (Identity x) =
        f x z
    foldl f z (Identity x) =
        f z x
    foldMap f (Identity x) =
        f x


main :: IO ()
main =
    do  print $ foldr (*) 1 (Identity 5)
        print $ foldl (*) 5 (Identity 5)
        print (foldMap (*5) (Identity 100) :: Product Integer)
