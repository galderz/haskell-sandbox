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


main :: IO ()
main =
    sample trivialGen
