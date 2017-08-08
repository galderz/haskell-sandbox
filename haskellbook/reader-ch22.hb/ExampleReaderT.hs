import Control.Monad.Trans.Reader

withReaderT
    :: (r' -> r)
    -- ^ The function to modify the environment
    -> ReaderT r m a
    -- ^ Computation to run in the modified environment
    -> ReaderT r' m a
withReaderT f m =
    ReaderT $ runReaderT m . f
