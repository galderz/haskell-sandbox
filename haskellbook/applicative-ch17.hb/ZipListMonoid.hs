module ZipListMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- ZipList [] is Zero, not identity:
-- Zero
-- n * 0 == 0
-- Identity
-- n * 1 == n
instance Monoid a => Monoid (ZipList a) where
    mempty =
        pure mempty
    mappend =
        liftA2 mappend


instance Eq a => EqProp (ZipList a) where
    (=-=) =
        eq


main :: IO ()
main =
    quickBatch $ monoid (ZipList [1 :: Sum Int])
