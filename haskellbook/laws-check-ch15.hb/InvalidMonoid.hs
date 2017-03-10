module InvalidMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck


data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)


instance Arbitrary Bull where
    arbitrary =
        frequency   [ (1, return Fools)
                    , (1, return Twoo)]


instance Monoid Bull where
    mempty =
        Fools
    mappend _ _ =
        Fools


type BullMappend =
    Bull -> Bull -> Bull -> Bool


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
    (mempty <> a)
        == a


-- Test with:
-- quickCheck (monoidRightIdentity :: String -> Bool)
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
    (a <> mempty)
        == a


-- Run main
-- Invalid monoid because identity laws do not conform
-- Regardless of the input, always returns Fools which
-- does not obey the monoid laws
main :: IO ()
main =
    do  quickCheck (monoidAssoc :: BullMappend)
        quickCheck (monoidLeftIdentity :: Bull -> Bool)
        quickCheck (monoidRightIdentity :: Bull -> Bool)
