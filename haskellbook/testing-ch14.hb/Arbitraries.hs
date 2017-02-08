module Arbitraries where

import Test.QuickCheck


data Trivial =
    Trivial
    deriving (Eq, Show)


trivialGen :: Gen Trivial
trivialGen =
    return Trivial


instance Arbitrary Trivial where
    arbitrary =
        trivialGen


data Identity a =
    Identity a
    deriving (Eq, Show)


identityGen :: Arbitrary a => Gen (Identity a)
identityGen =
    do  a <- arbitrary
        return (Identity a)


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen


identityGenInt :: Gen (Identity Int)
identityGenInt =
    identityGen


main :: IO ()
main =
    sample identityGenInt
    -- sample trivialGen
