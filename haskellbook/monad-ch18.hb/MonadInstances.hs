module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Nope a =
    NopeDotJpg
    deriving (Eq, Show)


data PhhhbbtttEither b a =
    Lft a
    | Rght b
    deriving (Eq, Show)


instance Functor Nope where
    fmap _ _ =
        NopeDotJpg


instance Functor (PhhhbbtttEither b) where
    fmap f (Lft x) =
        Lft (f x)
    fmap _ (Rght b) =
        Rght b


instance Applicative Nope where
    pure _ =
        NopeDotJpg
    (<*>) _ _ =
        NopeDotJpg


instance Monoid b => Applicative (PhhhbbtttEither b) where
    pure x =
        Lft x
    (<*>) _ (Rght b) =
        Rght b
    (<*>) (Rght b) _ =
        Rght b
    (<*>) (Lft f) (Lft x) =
        Lft (f x)


instance Monad Nope where
    return =
        pure
    (>>=) _ _ =
        NopeDotJpg


instance Monoid b => Monad (PhhhbbtttEither b) where
    return =
        pure
    (>>=) (Rght b) _ =
        Rght b
    (>>=) (Lft x) f =
        f x


instance Arbitrary (Nope a) where
    arbitrary =
        return NopeDotJpg


instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            oneof [return $ Lft x, return $ Rght y]


instance (Eq a) => EqProp (Nope a) where
    (=-=) =
        eq


instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
    (=-=) =
        eq


main :: IO ()
main =
    do
        let
            testNope = undefined :: Nope (String, String, [Int])
            testEither = undefined :: PhhhbbtttEither
                (String, String, [Int]) (String, String, [Int])
        -- quickBatch $ functor testNope
        -- quickBatch $ applicative testNope
        quickBatch $ monad testNope
        quickBatch $ functor testEither
        quickBatch $ applicative testEither
        quickBatch $ monad testEither
