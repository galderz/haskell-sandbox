import Data.Bits
import Data.Word
import Test.Hspec


data IPAddress6 =
    IPAddress6 Word64 Word64
    deriving (Eq, Ord, Show)


-- 0:0:0:0:0:ffff:ac10:fe01
-- 0:0:0:0: 0:65535:44048:65025
-- 0 << 112
-- 0 << 96
-- 0 << 80
-- 0 << 64
-- 0 << 48
-- 65535 << 32
-- 44048 << 16
-- 65025 << 0


main :: IO ()
main =
    hspec $
    do
        describe "Shifting numbers: " $ do
            it "can calculate an IPv6 address by shifting and adding" $ do
                let dec =
                        (shift 0 112)
                        + (shift 0 96)
                        + (shift 0 80)
                        + (shift 0 64)
                        + (shift 0 48)
                        + (shift 65535 32)
                        + (shift 44048 16)
                        + 65025 :: Word64
                dec `shouldBe` 281473568538113
