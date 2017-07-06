import Data.Monoid


data Constant a b =
    Constant a


data Two a b =
    Two a b


data Three a b c =
    Three a b c


data Three' a b =
    Three' a b b


data Four' a b =
    Four' a b b b


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

    foldMap f (Three a b x) =
        f x


instance Foldable (Three' a) where
    foldl f z (Three' a x x') =
        f (f z x) x'

    foldr f z (Three' a x x') =
        f x (f x' z)

    foldMap f (Three' a x x')=
        mappend (f x) (f x')


instance Foldable (Four' a) where
    foldl f z (Four' a x x' x'')=
        f (f (f z x) x') x''

    foldr f z (Four' a x x' x'') =
        f x (f x' (f x'' z))

    foldMap f (Four' a x x' x'') =
        mappend (f x) (mappend (f x') (f x''))


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
        print $ Product 25 == (foldMap (*5) (Three 1 2 5) :: Product Integer)
        print $ Product 5 == (foldMap (*5) (Three 5 2 1) :: Product Integer)
        print $ Sum 5 == (foldMap (*5) (Three 100 2 1) :: Sum Integer)
        print $ Sum 500 == (foldMap (*5) (Three 1 2 100) :: Sum Integer)
        -- ((-) ((-) 5 2) 5) = (5 - 2) - 5
        print $ (-2) == (foldl (-) 5 $ Three' 1 2 5)
        -- (-) ((-) 5 2) 1 = (5 - 2) - 1
        print $ 2 == (foldl (-) 5 $ Three' 5 2 1)
        -- (-) 2 ((-) 5 5) = 2 - (5 - 5)
        print $ 2 == (foldr (-) 5 $ Three' 1 2 5)
        -- (-) 2 ((-) 1 5) = 2 - (1 - 5)
        print $ 6 == (foldr (-) 5 $ Three' 5 2 1)
        -- 7 = (5 - 2) + (5 - 1)
        print $ Sum 7 == (foldMap ((-) 5) (Three' 100 2 1) :: Sum Integer)
        -- -92 = (5 - 100) + (5 - 2)
        print $ Sum (-92) == (foldMap ((-) 5) (Three' 1 2 100) :: Sum Integer)
        -- (-) ((-) ((-) 5 2) 5) 3 = ((5 - 2) - 5) - 3
        print $ (-5) == (foldl (-) 5 $ Four' 1 2 5 3)
        -- (-) ((-) ((-) 5 2) 1) 3 = ((5 - 2) - 1) - 3
        print $ (-1) == (foldl (-) 5 $ Four' 5 2 1 3)
        -- (-) 2 ((-) 5 ((-) 3 5)) = 2 - (5 - (3 - 5))
        print $ (-5) == (foldr (-) 5 $ Four' 1 2 5 3)
        -- (-) 2 ((-) 1 ((-) 3 5)) = 2 - (1 - (3 - 5))
        print $ (-1) == (foldr (-) 5 $ Four' 5 2 1 3)
        -- 9 = (5 - 2) + (5 - 1) + (5 - 3)
        print $ Sum 9 == (foldMap ((-) 5) (Four' 100 2 1 3) :: Sum Integer)
        -- -90 = (5 - 2) + (5 - 100) + (5 - 3)
        print $ Sum (-90) == (foldMap ((-) 5) (Four' 1 2 100 3) :: Sum Integer)
