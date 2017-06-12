module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Nope a =
    NopeDotJpg
    deriving (Eq, Show)


instance Functor Nope where
    fmap _ _ =
        NopeDotJpg


instance Arbitrary (Nope a) where
    arbitrary =
        return NopeDotJpg


instance (Eq a) => EqProp (Nope a) where
    (=-=) =
        eq


main :: IO ()
main =
    do
        let
            testNode = undefined :: Nope (String, String, [Int])
        quickBatch $ functor testNode
