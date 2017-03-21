module MonoidIdentity where

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck


newtype Identity a =
    Identity a
    deriving (Eq, Show)


instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y =
        Identity (x <> y)


instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty =
        Identity mempty

    mappend = (<>)


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) ==
        ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a =
    (mempty <> a) ==
        a


monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a =
    (a <> mempty) ==
        a


type IdentityAssoc =
    Identity String
    -> Identity String
    -> Identity String
    -> Bool


main :: IO ()
main =
    do  quickCheck (semigroupAssoc :: IdentityAssoc)
        quickCheck (monoidLeftIdentity :: Identity String -> Bool)
        quickCheck (monoidRightIdentity :: Identity String -> Bool)
