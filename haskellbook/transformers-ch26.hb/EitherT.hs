{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
     EitherT { runEitherT :: m (Either e a) }


instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT emx) =
        EitherT $ (fmap . fmap) f emx
