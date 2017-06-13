module MonadInstances where

-- import Prelude hiding (Left, Right)

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


newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)


data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)


instance Functor Nope where
    fmap _ _ =
        NopeDotJpg


instance Functor (PhhhbbtttEither b) where
    fmap f (Lft x) =
        Lft (f x)
    fmap _ (Rght b) =
        Rght b


instance Functor Identity where
    fmap f (Identity x) =
        Identity (f x)


instance Functor List where
    fmap _ Nil =
        Nil
    fmap f (Cons x xs) =
        Cons (f x) (fmap f xs)


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


instance Applicative Identity where
    pure =
        Identity
    (<*>) (Identity f) (Identity x) =
        Identity (f x)


instance Monoid (List a) where
    mempty =
        Nil
    mappend Nil ca =
        ca
    mappend ca Nil =
        ca
    mappend (Cons x xs) ys =
        Cons x (mappend xs ys)


instance Applicative List where
    pure x =
        Cons x Nil
    (<*>) _ Nil =
        Nil
    (<*>) Nil _ =
        Nil
    (<*>) (Cons f fs) ca =
        -- Cons (f x) (fs <*> xs)
        mappend (fmap f ca) (fs <*> ca)


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


instance Monad Identity where
    return =
        pure
    (>>=) (Identity x) f =
        f x


instance Arbitrary (Nope a) where
    arbitrary =
        return NopeDotJpg


instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            oneof [return $ Lft x, return $ Rght y]


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do  x <- arbitrary
            return $ Identity x


instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        sized arbList


arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 =
    return Nil
arbList n =
    frequency [
        (1, return Nil)
        , (4, fmap Cons arbitrary <*> (arbList (n - 1)))
    ]


instance (Eq a) => EqProp (Nope a) where
    (=-=) =
        eq


instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
    (=-=) =
        eq


instance (Eq a) => EqProp (Identity a) where
    (=-=) =
        eq


instance (Eq a) => EqProp (List a) where
    (=-=) =
        eq


main :: IO ()
main =
    do
        let
            testNope = undefined :: Nope (String, String, [Int])
            testEither = undefined :: PhhhbbtttEither
                (String, String, [Int]) (String, String, [Int])
            testIdentity = undefined :: Identity (String, String, [Int])
            testList = undefined :: List (String, String, [Int])
        -- quickBatch $ functor testNope
        -- quickBatch $ applicative testNope
        -- quickBatch $ monad testNope
        --
        -- quickBatch $ functor testEither
        -- quickBatch $ applicative testEither
        -- quickBatch $ monad testEither
        --
        -- quickBatch $ functor testIdentity
        -- quickBatch $ applicative testIdentity
        -- quickBatch $ monad testIdentity
        --
        quickBatch $ functor testList
        quickBatch $ applicative testList
