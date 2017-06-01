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


data Three a b c =
    Three a b c
    deriving (Eq, Show)


data Three' a b =
    Three' a b b
    deriving (Eq, Show)


instance Functor Pair where
    fmap f (Pair x y) =
        Pair (f x) (f y)


instance Functor (Two a) where
    fmap f (Two a x) =
        Two a (f x)


instance Functor (Three a b) where
    fmap f (Three a b x)=
        Three a b (f x)


instance Functor (Three' a) where
    fmap f (Three' a x y) =
        Three' a (f x) (f y)


instance Applicative Pair where
    pure x =
        Pair x x
    (<*>) (Pair f g) (Pair x y) =
        Pair (f x) (g y)


instance Monoid a => Applicative (Two a) where
    pure x =
        Two mempty x
    (<*>) (Two a f) (Two a' x) =
        Two (mappend a a') (f x)


instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x =
        Three mempty mempty x
    (<*>) (Three a b f) (Three a' b' x) =
        Three (mappend a a') (mappend b b') (f x)


instance (Monoid a) => Applicative (Three' a) where
    pure x =
        Three' mempty x x
    (<*>) (Three' a f g) (Three' a' x y) =
        Three' (mappend a a') (f x) (g y)


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


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            z <- arbitrary
            return $ Three x y z


instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            return $ Three' x y y


instance Eq a => EqProp (Pair a) where
    (=-=) =
        eq


instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) =
        eq


instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) =
        eq


instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) =
        eq


main :: IO ()
main =
    do
        -- quickBatch $ functor (undefined :: Pair (String, String, Int))
        quickBatch $ applicative (undefined :: Pair (String, String, Int))
        -- quickBatch $ functor (undefined :: Two (String, String, Int) (String, String, Int))
        quickBatch $ applicative (undefined :: Two (String, String, [Int]) (String, String, [Int]))
        -- quickBatch $ functor (undefined :: Three (String, String, Int) (String, String, Int) (String, String, Int))
        quickBatch $ applicative (undefined :: Three (String, String, [Int]) (String, String, [Int]) (String, String, [Int]))
        -- quickBatch $ functor (undefined :: Three' (String, String, Int) (String, String, Int))
        quickBatch $ applicative (undefined :: Three' (String, String, [Int]) (String, String, [Int]))
