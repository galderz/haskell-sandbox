import Control.Applicative (liftA, liftA2)
import Control.Monad (join, liftM, liftM2, liftM3)

andOne x =
    [x, 1]


-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma =
    join $ fmap f ma


main :: IO ()
main =
    do  -- You can derive Functor in terms of Monad
        -- fmap f xs = xs >>= return . f
        print $ fmap (+1) [1..3]
        print $ [1..3] >>= return . (+1)
        -- Monad, in a sense, is a generalization of concat
        print $ andOne 10
        print $ fmap andOne [4, 5, 6]
        print $ concat $ fmap andOne [4, 5, 6]
        -- liftA is just fmap with Applicative typeclass constraint
        print $ liftA andOne [4, 5, 6]
        -- liftM is just fmap with Monad typeclass constraint
        print $ liftM andOne [4, 5, 6]
        -- liftA2
        print $ liftA2 (,) (Just 3) (Just 5)
        -- liftM2
        print $ liftM2 (,) (Just 3) (Just 5)
        -- liftA2 vs zipWith
        -- differ in how the list monoid is used
        print $ zipWith (+) [3, 4] [5, 6]
        print $ liftA2 (+) [3, 4] [5, 6]
        -- liftM3 vs zipWith3
        print $ zipWith3 (,,) [1, 2] [3] [5, 6]
        print $ liftM3 (,,) [1, 2] [3] [5, 6]
