import Data.Bits
import Data.Word
import Test.Hspec


data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord, Show)


main :: IO ()
main =
    hspec $
    do
        describe "Shifting numbers: " $ do
            it "can calculate an IP address by shifting and adding numbers" $ do
                let dec =
                        (shift 172 24)
                        + (shift 16 16)
                        + (shift 254 8)
                        + 1 :: Word32
                dec `shouldBe` 2886794753
