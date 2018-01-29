newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }


newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)
