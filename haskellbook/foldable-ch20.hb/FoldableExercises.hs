import Data.Monoid


data Constant a b =
    Constant a


data Two a b =
    Two a b


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


instance Foldable (Two a) where
    foldl f z (Two a x) =
        f z x

    foldr =
        undefined

    foldMap =
        undefined


main :: IO ()
main =
    do  print $ 5 == foldl (*) 5 (Constant 5)
        print $ 5 == foldr (*) 5 (Constant 5)
        print $ Product 1 == (foldMap (*5) (Constant 100) :: Product Integer)
        print $ Sum 0 == (foldMap (*5) (Constant 100) :: Sum Integer)
        print $ 25 == (foldl (*) 5 $ Two 1 5)
        print $ 5 == (foldl (*) 5 $ Two 5 1)
