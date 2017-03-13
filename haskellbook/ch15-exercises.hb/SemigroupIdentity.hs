module SemigroupIdentity where

import Data.Semigroup
import Test.QuickCheck


newtype Identity a =
    Identity a
    deriving (Eq, Show)


instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y =
        Identity (x <> y)


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type IdentityAssoc =
    Identity String -> Identity String -> Identity String -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: IdentityAssoc)
