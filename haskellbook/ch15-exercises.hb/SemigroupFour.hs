module SemigroupFour where

import Data.Semigroup
import Test.QuickCheck


data Four a b c d =
    Four a b c d
    deriving (Eq, Show)


instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
    Four a b c d <> Four a' b' c' d' =
        Four (a <> a') (b <> b') (c <> c') (d <> d')


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        zz <- arbitrary
        return $ Four x y z zz


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type FourAssoc =
    Four String Ordering String Ordering
    -> Four String Ordering String Ordering
    -> Four String Ordering String Ordering
    -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: FourAssoc)
