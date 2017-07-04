import Data.Monoid


data Constant a b =
    Constant a


instance Foldable (Constant a) where
    foldl f z (Constant x) =
        -- Can't be `f z x` because x is part of the container
        -- and hence it cannot be used.
        -- Only `b` is usable but that's not part of instance
        -- , so the only thing you can do is return z
        z

    foldr f z (Constant _) =
        -- Same as for foldl
        z

    foldMap f (Constant _) =
        mempty


main :: IO ()
main =
    do  print $ 5 == foldl (*) 5 (Constant 5)
        print $ 5 == foldr (*) 5 (Constant 5)
        print $ Product 1 == (foldMap (*5) (Constant 100) :: Product Integer)
        print $ Sum 0 == (foldMap (*5) (Constant 100) :: Sum Integer)
