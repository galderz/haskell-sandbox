{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Monad (join, liftM2)


j :: Monad m => m (m a) -> m a
j =
    join


l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m =
    fmap f m


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 =
    liftM2


main :: IO ()
main =
    do  print $ j [[1, 2], [], [3]]
        print $ j (Just (Just 1))
        print $ j ((Just Nothing) :: Maybe (Maybe Int))
        print $ j (Nothing :: Maybe (Maybe Int))
        print $ l1 (+1) (Just 1)
        print $ l1 (+1) Nothing
        print $ l2 (+) (Just 1) (Just 2)
        print $ l2 (+) (Just 1) Nothing
        print $ l2 (+) Nothing Nothing
