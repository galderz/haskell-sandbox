module MonoidBoolConj where

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck


newtype BoolConj =
    BoolConj Bool
    deriving (Eq, Show)


instance Semigroup BoolConj where
    BoolConj True <> BoolConj True =
        BoolConj True

    _ <> _ =
        BoolConj False


instance Monoid BoolConj where
    mempty =
        BoolConj True

    mappend = (<>)


instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return $ BoolConj x


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) ==
        ((a <> b) <> c)


monoidLeft :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeft a =
    (mempty <> a) ==
        a


monoidRight :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRight a =
    (a <> mempty) ==
        a


type BoolConjAssoc =
    BoolConj
    -> BoolConj
    -> BoolConj
    -> Bool


main :: IO ()
main =
    do  quickCheck (semigroupAssoc :: BoolConjAssoc)
        quickCheck (monoidLeft :: BoolConj -> Bool)
        quickCheck (monoidRight :: BoolConj -> Bool)
