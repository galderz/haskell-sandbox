newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }


newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)


instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga


instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap) f ma


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x =
        Compose $ (pure . pure) x

    (<*>) (Compose f) (Compose a) =
        Compose $ (fmap (<*>) f) <*> a


instance (Applicative m) => Applicative (MaybeT m) where
    pure x =
        MaybeT $ (pure . pure) x

    (<*>) (MaybeT f) (MaybeT a) =
        MaybeT $ (fmap (<*>) f) <*> a


newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) =
        Identity (f a)


instance Applicative Identity where
    pure =
        Identity

    (Identity f) <*> (Identity a) =
        Identity (f a)

