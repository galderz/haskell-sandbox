{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
     EitherT { runEitherT :: m (Either e a) }


instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT emx) =
        EitherT $ (fmap . fmap) f emx


instance Applicative m => Applicative (EitherT e m) where
    pure x =
        EitherT $ (pure . pure) x

    (<*>) (EitherT emf) (EitherT emx) =
        EitherT $ (fmap (<*>) emf) <*> emx
