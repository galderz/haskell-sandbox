module MonoidCompHspec where

import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

newtype Comp a =
    Comp { unComp :: (a -> a) }


instance Semigroup (Comp a) where
    Comp f <> Comp g =
        Comp (g . f)


instance Monoid (Comp a) where
    mempty =
        Comp id

    mappend = (<>)


f :: Comp Integer
f =
    Comp $ \n -> n + 1


g :: Comp Integer
g =
    Comp $ \n -> n - 1


main :: IO ()
main = hspec $ do
    describe "Comp" $ do
        it "unComp applied on (-1) and (+1) on 0 is equal to 0" $ do
            (unComp (f <> g) $ 0) `shouldBe` 0
        it "unComp applied on (-1) and (+1) on 1 is equal to 1" $ do
            (unComp (f <> g) $ 1) `shouldBe` 1
        it "unComp applied on (+1) and (+1) on 1 is equal to 3" $ do
            (unComp (f <> f) $ 1) `shouldBe` 3
        it "unComp applied on (+1) and (-1) on 1 is equal to 1" $ do
            (unComp (g <> f) $ 1) `shouldBe` 1
        it "unComp applied on (+1) on 1 with mempty is equal 2" $ do
            (unComp (mappend f mempty) $ 1) `shouldBe` 2
