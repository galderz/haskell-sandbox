import Data.Monoid
import Optional
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Optional" $ do
        it "mappend of Sum and Nada is Sum" $ do
            Only (Sum 1) <> Nada `shouldBe` Only (Sum 1)
        it "mappend of List and Nada is List" $ do
            Only [1] <> Nada `shouldBe` Only [1]
        it "mappend of Nada and Sum is Sum" $ do
            Nada <> Only (Sum 1) `shouldBe` Only (Sum 1)
