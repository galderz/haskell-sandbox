module MonoidTwo where

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Test.QuickCheck


data Two a b =
    Two a b
    deriving (Eq, Show)


instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two x x' <> Two y y' =
        Two (x <> y) (x' <> y')


instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty =
        Two mempty mempty

    mappend = (<>)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) ==
        ((a <> b) <> c)


monoidLeftTwo :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftTwo a =
    (mempty <> a) ==
        a


monoidRightTwo :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightTwo a =
    (a <> mempty) ==
        a


type TwoAssoc =
    Two String String
    -> Two String String
    -> Two String String
    -> Bool


main :: IO ()
main =
    do  quickCheck (semigroupAssoc :: TwoAssoc)
        quickCheck (monoidLeftTwo :: Two String String -> Bool)
        quickCheck (monoidRightTwo :: Two String String -> Bool)
