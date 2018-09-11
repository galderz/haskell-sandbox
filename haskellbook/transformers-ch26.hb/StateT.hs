import Data.Bifunctor


newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) =
        StateT $
            \s ->
                 let r =
                         sma s
                 in
                     fmap (first f) r


instance Monad m => Applicative (StateT s m) where
    pure x =
        StateT $ \s -> pure (x, s)

    (<*>) (StateT smf) (StateT smx) =
        StateT $ \s ->
            do
                (f, s') <- smf s
                (y, s'') <- smx s'
                return (f y, s'')


instance Monad m => Monad (StateT s m) where
    return =
        pure

    (>>=) =
        undefined
