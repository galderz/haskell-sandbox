module SumMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)


instance Functor (Sum a) where
    fmap _ (First a) =
        First a
    fmap f (Second x) =
        Second (f x)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            oneof [return $ First x, return $ Second y]


instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) =
        eq


type TestSum = Sum (String, String, Int) (String, String, Int)


main :: IO ()
main =
    quickBatch $ functor (undefined :: TestSum)
