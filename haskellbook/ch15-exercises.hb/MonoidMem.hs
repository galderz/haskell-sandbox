module MonoidMem where

import Data.Monoid
import Test.Hspec


newtype Mem s a =
    Mem {
        runMem :: s -> (a, s)
    }


instance Monoid a => Monoid (Mem s a) where
    mempty =
        Mem (\s -> (mempty, s))

    mappend (Mem f) (Mem g) =
        Mem (\s ->
            let
                (a, x) = f s
                (b, y) = g x
            in
                (a <> b, y)
        )


f' :: Mem Integer [Char]
f' =
    Mem $ \s -> ("hi", s + 1)


main :: IO ()
main = hspec $ do
    describe "Mem" $ do
        it "runMem right identity with a function" $ do
            (runMem (f' <> mempty) 0) `shouldBe` ("hi", 1)
        it "runMem left identity with a function" $ do
            (runMem (mempty <> f') 0) `shouldBe` ("hi", 1)
        it "runMem left identity with a value" $ do
            (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0)
        it "runMem right identity is same as applying function directly" $ do
            (runMem (f' <> mempty) 0 == runMem f' 0) `shouldBe` True
        it "runMem left identity is same as applying function directly" $ do
            (runMem (mempty <> f') 0 == runMem f' 0) `shouldBe` True
