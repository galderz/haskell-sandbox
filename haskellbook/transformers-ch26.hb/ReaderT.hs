newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }


instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rmx) =
        ReaderT $ (fmap . fmap) f rmx


instance Applicative m => Applicative (ReaderT r m) where
    pure x =
        ReaderT (pure . pure x)

    (<*>) (ReaderT rmf) (ReaderT rmx) =
        ReaderT ((fmap (<*>) rmf) <*> rmx)
