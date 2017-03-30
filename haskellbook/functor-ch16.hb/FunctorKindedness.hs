module FunctorKindedness where


data FixMePls a =
    FixMe
    | Pls a
    deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe =
        FixMe

    fmap f (Pls a) =
        Pls (f a)


fixMePlsEx =
    fmap (+1) (Pls 1)


-- data FixMePls =
--     FixMe
--     | Pls
--     deriving (Eq, Show)
--
-- Error: FixMePls not a `* -> *` type
-- instance Functor FixMePls where
--     fmap =
--         error "it doesn't matter, it won't compile"


-- Fails expecting fewer arguments
-- class Impish v where
--     impossibleKind :: v -> v a


-- Fails expecting one more argument
-- class AlsoImp v where
--     nope :: v a -> v
