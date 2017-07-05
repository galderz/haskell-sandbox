import Data.Monoid


data Constant a b =
    Constant a


data Two a b =
    Two a b


data Three a b c =
    Three a b c


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

    foldr f z (Two a x) =
        f x z

    foldMap f (Two a x) =
        f x


instance Foldable (Three a b) where
    foldl f z (Three a b x)=
        f z x

    foldr f z (Three a b x) =
        f x z

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
        print $ 25 == (foldr (*) 5 $ Two 1 5)
        print $ 5 == (foldr (*) 5 $ Two 5 1)
        print $ Product 25 == (foldMap (*5) (Two 1 5) :: Product Integer)
        print $ Product 5 == (foldMap (*5) (Two 5 1) :: Product Integer)
        print $ Sum 5 == (foldMap (*5) (Two 100 1) :: Sum Integer)
        print $ Sum 500 == (foldMap (*5) (Two 1 100) :: Sum Integer)
        print $ 25 == (foldl (*) 5 $ Three 1 2 5)
        print $ 5 == (foldl (*) 5 $ Three 5 2 1)
        print $ 25 == (foldr (*) 5 $ Three 1 2 5)
        print $ 5 == (foldr (*) 5 $ Three 5 2 1)
