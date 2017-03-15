module SemigroupCombine where

import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)


newtype Combine a b =
    Combine { unCombine :: (a -> b) }


instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine f' =
        Combine (f <> f')


f :: Combine Integer (Sum Integer)
f =
    Combine $ \n -> Sum (n + 1)


g :: Combine Integer (Sum Integer)
g =
    Combine $ \n -> Sum (n - 1)


main :: IO ()
main =
    do  print $ unCombine (f <> g) $ 0 -- Sum {getSum = 0}: (0 - 1) <> (0 + 1)
        print $ unCombine (f <> g) $ 1 -- Sum {getSum = 2}: (1 - 1) <> (1 + 1)
        print $ unCombine (f <> f) $ 1 -- Sum {getSum = 4}: (1 + 1) <> (1 + 1)
        print $ unCombine (g <> f) $ 1 -- Sum {getSum = 2}: (1 + 1) <> (1 - 1)
