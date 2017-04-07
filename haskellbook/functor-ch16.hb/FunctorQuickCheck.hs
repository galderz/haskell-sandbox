module FunctorQuickCheck where

import Test.QuickCheck


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
    fmap id f == f


functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x=
    (fmap g (fmap f x)) == (fmap (g . f) x)


main :: IO ()
main =
    do  quickCheck $ \x -> functorIdentity (x :: [Int])
        quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
