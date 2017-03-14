module SemigroupBoolConj where

import Data.Semigroup
import Test.QuickCheck


newtype BoolConj =
    BoolConj Bool
    deriving (Eq, Show)


instance Semigroup BoolConj where
    BoolConj True <> BoolConj True =
        BoolConj True

    _ <> _ =
        BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return $ BoolConj x


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: BoolConjAssoc)
