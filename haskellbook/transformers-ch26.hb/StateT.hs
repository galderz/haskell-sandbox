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


instance Applicative m => Applicative (StateT s m) where
    pure x =
        StateT $ \s -> pure (x, s)

    (<*>) =
        undefined
