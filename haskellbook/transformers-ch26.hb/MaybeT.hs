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

    (<*>) =
        undefined
