{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Monad (forM, join, liftM2)
import Data.List


j :: Monad m => m (m a) -> m a
j =
    join


l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m =
    fmap f m


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 =
    liftM2


a :: Monad m => m a -> m (a -> b) -> m b
a ma mf =
    mf <*> ma


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ =
    pure []
meh (x:xs) f =
    (fmap (++) (fmap (: []) (f x))) <*> (meh xs f)
    -- fmap (++) (fmap (: []) (f x)) (meh xs f)
    -- mappend (meh xs f) (fmap (: []) (f x))
    -- mappend ((pure insert) <*> (f x)) (meh xs f)
    -- mappend (f x) (meh xs f)


meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' =
    forM


flipType :: (Monad m) => [m a] -> m [a]
flipType xs =
    meh xs id


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
        print $ a (Just 1) (Just (+1))
        print $ a Nothing (Just (+1))
        print $ meh [1..10] (Just)
        print $ meh' [1..10] (Just)
        print $ flipType [Just 1, Just 2, Just 3]
