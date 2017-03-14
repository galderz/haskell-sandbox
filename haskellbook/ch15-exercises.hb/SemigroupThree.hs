module SemigroupThree where

import Data.Semigroup
import Test.QuickCheck


data Three a b c =
    Three a b c
    deriving (Eq, Show)


instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three a b c <> Three a' b' c' =
        Three (a <> a') (b <> b') (c <> c')


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three x y z


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type ThreeAssoc =
    Three String Ordering String
    -> Three String Ordering String
    -> Three String Ordering String
    -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: ThreeAssoc)
