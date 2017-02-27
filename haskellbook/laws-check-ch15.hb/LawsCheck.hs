module LawsCheck where

import Data.Monoid
import Test.QuickCheck


-- Test with:
-- type S = String
-- type B = Bool
-- quickCheck (monoidAssoc :: S -> S -> S -> B)
--
-- To find out more what's being tested:
-- veboseCheck monoidAssoc
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)


-- Test with:
-- quickCheck (monoidLeftIdentity :: String -> Bool)
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
    (mempty <> a) == a


-- Test with:
-- quickCheck (monoidRightIdentity :: String -> Bool)
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
    (a <> mempty) == a
