-- Plain old Identity. 'a' can be
-- something with more structure,
-- but it's not required and Identity
-- won't know anything about it.
newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)


-- The identity monad transformer, serving
-- only to to specify that additional
-- structure should exist.
newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) =
        Identity (f a)


instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) =
        IdentityT (fmap f fa)
