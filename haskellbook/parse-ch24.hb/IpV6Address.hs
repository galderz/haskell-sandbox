import Data.Bits
import Data.Char
import Data.Word
import Test.Hspec
import Text.Trifecta


data IPAddress6 =
    IPAddress6 Word64 Word64
    deriving (Eq, Ord, Show)


parseHexDigit :: Parser Char
parseHexDigit =
    oneOf (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])


stringToInt :: String -> Int
stringToInt s =
    foldl (\z x -> (z * 16) + digitToInt x) 0 s


base16Int :: Parser Int
base16Int =
    fmap stringToInt (some parseHexDigit)


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) =
    Just a
maybeSuccess _ =
    Nothing


toWord64 :: Int -> Int -> Int -> Int -> Int -> Word64
toWord64 padding i1 i2 i3 i4 =
    fromIntegral $ (shift i1 (48 + padding))
        + (shift i2 (32 + padding))
        + (shift i3 (16 + padding))
        + shift i4 (0 + padding)


toDecimal :: IPAddress6 -> Word64
toDecimal (IPAddress6 high low) =
    high + low


parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 =
    do  ip1 <- base16Int
        _ <- char ':'
        ip2 <- base16Int
        _ <- char ':'
        ip3 <- base16Int
        _ <- char ':'
        ip4 <- base16Int
        _ <- char ':'
        ip5 <- base16Int
        _ <- char ':'
        ip6 <- base16Int
        _ <- char ':'
        ip7 <- base16Int
        _ <- char ':'
        ip8 <- base16Int
        return $ IPAddress6
            (toWord64 64 ip1 ip2 ip3 ip4)
            (toWord64 0 ip5 ip6 ip7 ip8)


main :: IO ()
main =
    hspec $
    do
        describe "IPv6 parsing: " $ do
            it "can parse 0:0:0:0:0:ffff:cc78:f" $ do
                let p =
                        parseIPAddress6
                    i =
                        "0:0:0:0:0:ffff:cc78:f"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just (IPAddress6 0 281474112159759)
            it "can parse 0:0:0:0:0:ffff:ac10:fe01" $ do
                let p =
                        parseIPAddress6
                    i =
                        "0:0:0:0:0:ffff:ac10:fe01"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just (IPAddress6 0 281473568538113)
        describe "Base16Int parsing: " $ do
            it "can parse fe01" $ do
                let p =
                        base16Int
                    i =
                        "fe01"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just 65025
            it "can parse f" $ do
                let p =
                        base16Int
                    i =
                        "f"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just 15
            it "can parse 7" $ do
                let p =
                        base16Int
                    i =
                        "7"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just 7
            it "can parse A" $ do
                let p =
                        base16Int
                    i =
                        "a"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just 10
        describe "Decimal parsing: " $ do
            it "can parse 1" $ do
                let p =
                        digit
                    i =
                        "1"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just '1'
        describe "Shifting numbers: " $ do
            it "can calculate lower IPv6 address number using toWord64" $ do
                let dec =
                        toWord64 0 0 65535 44048 65025
                dec `shouldBe` 281473568538113
            it "can calculate an IPv6 address by shifting and adding" $ do
                let dec =
                        (shift 0 112)
                        + (shift 0 96)
                        + (shift 0 80)
                        + (shift 0 64) :: Word64
                dec `shouldBe` 0
            it "can calculate lower IPv6 address number" $ do
                let dec =
                        (shift 0 48)
                        + (shift 65535 32)
                        + (shift 44048 16)
                        + 65025 :: Word64
                dec `shouldBe` 281473568538113
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
