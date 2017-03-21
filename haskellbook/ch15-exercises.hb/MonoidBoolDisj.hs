module MonoidBoolDisj where

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck


newtype BoolDisj =
    BoolDisj Bool
    deriving (Eq, Show)


instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False =
        BoolDisj False

    _ <> _ =
        BoolDisj True


instance Monoid BoolDisj where
    mempty =
        BoolDisj False

    mappend = (<>)


instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return $ BoolDisj x


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


type BoolDisjAssoc =
    BoolDisj
    -> BoolDisj
    -> BoolDisj
    -> Bool


main :: IO ()
main =
    do  quickCheck (semigroupAssoc :: BoolDisjAssoc)
        quickCheck (monoidLeft :: BoolDisj -> Bool)
        quickCheck (monoidRight :: BoolDisj -> Bool)
