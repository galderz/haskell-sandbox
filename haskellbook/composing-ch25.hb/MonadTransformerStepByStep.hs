{-# LANGUAGE InstanceSigs #-}

import Control.Monad


-- The identity monad transformer, serving
-- only to to specify that additional
-- structure should exist.
newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)


instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) =
        IdentityT (fmap f fa)


instance (Applicative m) => Applicative (IdentityT m) where
    pure x =
        IdentityT (pure x)

    (IdentityT fab) <*> (IdentityT fa) =
        IdentityT (fab <*> fa)


instance (Monad m) => Monad (IdentityT m) where
    return =
        pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        let
            -- fmap and join is >>=
            aimb = ma >>= runIdentityT . f

            -- Functor law:
            -- fmap (f . g) == fmap f . fmap g
            -- aimb = join (fmap (runIdentityT . f) ma)
        in
            IdentityT aimb

        -- Baseline version
        -- let
        --     aimb = join (fmap runIdentityT (fmap f ma))
        -- in
        --     IdentityT aimb
