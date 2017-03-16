module SemigroupCombineHspec where

import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.Hspec
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
main = hspec $ do
    describe "Comp" $ do
        it "unCombine applied on (-1) and (+1) on 0 is equal to Sum(0)" $ do
            (unCombine (f <> g) $ 0) `shouldBe` Sum 0 -- (0 - 1) <> (0 + 1)
        it "unCombine applied on (-1) and (+1) on 1 is equal to Sum(2)" $ do
            (unCombine (f <> g) $ 1) `shouldBe` Sum 2 -- (1 - 1) <> (1 + 1)
        it "unCombine applied on (+1) and (+1) on 1 is equal to Sum(4)" $ do
            (unCombine (f <> f) $ 1) `shouldBe` Sum 4 -- (1 + 1) <> (1 + 1)
        it "unCombine applied on (+1) and (-1) on 1 is equal to Sum (2)" $ do
            (unCombine (g <> f) $ 1) `shouldBe` Sum 2 -- (1 + 1) <> (1 - 1)
