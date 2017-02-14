module Failure where

import Test.QuickCheck


square :: Num a => a -> a
square x =
    x * x


squareIdentity :: Double -> Double
squareIdentity =
    square . sqrt


prop_SquareSqrt :: Double -> Bool
prop_SquareSqrt x =
    squareIdentity x == x


main :: IO ()
main =
    quickCheck prop_SquareSqrt
