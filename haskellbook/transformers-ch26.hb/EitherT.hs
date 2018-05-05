{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)


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


instance Monad m => Monad (EitherT e m) where
    return =
        pure

    (EitherT ma) >>= f =
        EitherT $
            do  v <- ma
                case v of
                    Left x ->
                        return $ Left x
                    Right y ->
                        runEitherT (f y)


-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT emx) =
    EitherT $ fmap swapEither emx


swapEither :: Either e a -> Either a e
swapEither (Left x) =
    Right x
swapEither (Right y) =
    Left y


eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fx fy (EitherT ema) =
    ema >>= either' fx fy


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fx _ (Left x) =
    fx x
either' _ fy (Right y) =
    fy y
