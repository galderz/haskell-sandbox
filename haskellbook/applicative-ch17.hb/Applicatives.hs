module Applicatives where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Pair a =
    Pair a a
    deriving (Eq, Show)


data Two a b =
    Two a b
    deriving (Eq, Show)


instance Functor Pair where
    fmap f (Pair x y) =
        Pair (f x) (f y)


instance Functor (Two a) where
    fmap f (Two x y) =
        Two x (f y)


instance Applicative Pair where
    pure x =
        Pair x x
    (<*>) (Pair f g) (Pair x y) =
        Pair (f x) (g y)


instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            return $ Pair x y


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            return $ Two x y


instance Eq a => EqProp (Pair a) where
    (=-=) =
        eq


instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) =
        eq


main :: IO ()
main =
    do
        -- quickBatch $ functor (undefined :: Pair (String, String, Int))
        quickBatch $ applicative (undefined :: Pair (String, String, Int))
        quickBatch $ functor (undefined :: Two (String, String, Int) (String, String, Int))
