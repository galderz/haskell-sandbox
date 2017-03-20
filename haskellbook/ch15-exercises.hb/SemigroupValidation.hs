module SemigroupValidation where

import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, oneof, quickCheck)


data Validation a b =
    Failure a
    | Success b
    deriving (Eq, Show)


newtype AccumulateRight a b =
    AccumulateRight (Validation a b)
    deriving (Eq, Show)


instance Semigroup a =>
    Semigroup (Validation a b) where
        Failure a <> Failure a' =
            Failure (a <> a')

        Failure a <> _ =
            Failure a

        Success _ <> Failure a =
            Failure a

        a <> _ =
            a


instance Semigroup b =>
    Semigroup (AccumulateRight a b) where
        a@(AccumulateRight (Failure _)) <> _ =
            a

        _ <> a@(AccumulateRight (Failure _)) =
            a

        AccumulateRight (Success b) <> AccumulateRight (Success b') =
            AccumulateRight (Success (b <> b'))


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure a, return $ Success b]


instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof   [return $ AccumulateRight (Failure a)
                , return $ AccumulateRight (Success b)]


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type ValidationAssoc =
    Validation String String -> Validation String String -> Validation String String -> Bool


type AccumulateRightAssoc =
    AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool


main :: IO ()
main =
    do  quickCheck (semigroupAssoc :: ValidationAssoc)
        quickCheck (semigroupAssoc :: AccumulateRightAssoc)
