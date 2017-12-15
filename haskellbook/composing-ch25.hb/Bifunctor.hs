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

    second =
        undefined
