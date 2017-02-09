module UsingQuickCheck where

import Test.QuickCheck


half :: Fractional a => a -> a
half x =
    x / 2


halfIdentity :: Fractional a => a -> a
halfIdentity =
    (*2) . half


prop_half :: Float -> Bool
prop_half x =
    halfIdentity x == x


main :: IO ()
main =
    quickCheck prop_half
