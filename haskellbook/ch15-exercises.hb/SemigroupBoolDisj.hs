module SemigroupBoolDisj where

import Data.Semigroup
import Test.QuickCheck


newtype BoolDisj =
    BoolDisj Bool
    deriving (Eq, Show)


instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False =
        BoolDisj False

    _ <> _ =
        BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return $ BoolDisj x


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
