module MonoidTrivial where

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck

data Trivial =
    Trivial deriving (Eq, Show)


instance Semigroup Trivial where
    _ <> _ =
        Trivial


instance Monoid Trivial where
    mempty = undefined

    mappend = (<>)


instance Arbitrary Trivial where
    arbitrary =
        return Trivial


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


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


main :: IO ()
main =
    do  quickCheck (semigroupAssoc :: TrivialAssoc)
        quickCheck (monoidLeftIdentity :: Trivial -> Bool)
        quickCheck (monoidRightIdentity :: Trivial -> Bool)
