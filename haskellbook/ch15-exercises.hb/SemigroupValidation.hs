module SemigroupValidation where

import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, oneof, quickCheck)
--import Test.QuickCheck


data Validation a b =
    Failure a
    | Success b
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


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure a, return $ Success b]


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type ValidationAssoc =
    Validation String String -> Validation String String -> Validation String String -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: ValidationAssoc)
