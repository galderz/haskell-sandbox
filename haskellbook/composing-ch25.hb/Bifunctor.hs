class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g =
        first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f =
        bimap f id

    second :: (b -> c) -> p a b -> p a c
    second =
        bimap id


data Deux a b =
    Deux a b


instance Bifunctor Deux where
    bimap f g (Deux x y) =
        Deux (f x) (g y)

    first f (Deux x y) =
        Deux (f x) y

    second f (Deux x y) =
        Deux x (f y)


data Const a b =
    Const a


instance Bifunctor Const where
    bimap f g (Const x) =
        Const (f x)

    first f (Const x) =
        Const (f x)

    second f (Const x) =
        Const x


data Drei a b c =
    Drei a b c


instance Bifunctor (Drei a) where
    bimap f g (Drei x y z) =
        Drei x (f y) (g z)

    first f (Drei x y z) =
        Drei x (f y) z

    second f (Drei x y z) =
        Drei x y (f z)


data SuperDrei a b c =
    SuperDrei a b


instance Bifunctor (SuperDrei a) where
    bimap f g (SuperDrei x y) =
        SuperDrei x (f y)

    first f (SuperDrei x y) =
        SuperDrei x (f y)

    second f (SuperDrei x y) =
        SuperDrei x y


data SemiDrei a b c =
    SemiDrei a


instance Bifunctor (SemiDrei a) where
    bimap f g (SemiDrei x) =
        SemiDrei x

    first f (SemiDrei x) =
        SemiDrei x

    second f (SemiDrei x) =
        SemiDrei x


data Quadriceps a b c d =
    Quadzzz a b c d


instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b x y) =
        Quadzzz a b (f x) (g y)

    first f (Quadzzz a b x y) =
        Quadzzz a b (f x) y

    second f (Quadzzz a b x y) =
        Quadzzz a b x (f y)


data MyEither a b =
    MyLeft a
    | MyRight b


instance Bifunctor MyEither where
    bimap f g (MyLeft x) =
        MyLeft (f x)
    bimap f g (MyRight y) =
        MyRight (g y)

    first f (MyLeft x) =
        MyLeft (f x)
    first f (MyRight y) =
        MyRight y

    second f (MyLeft x) =
        MyLeft x
    second f (MyRight y) =
        MyRight (f y)
