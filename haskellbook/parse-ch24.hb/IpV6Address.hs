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


toWord64 :: Int -> Int -> Int -> Int -> Word64
toWord64 i1 i2 i3 i4 =
    fromIntegral $ (shift i1 48)
        + (shift i2 32)
        + (shift i3 16)
        + i4


toDecimal :: IPAddress6 -> Word64
toDecimal (IPAddress6 high low) =
    high + low


sumHighLow :: Word64 -> Word64 -> Integer
sumHighLow n1 n2 =
    (shift (toInteger n1) 64) + (toInteger n2)


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
            (toWord64 ip1 ip2 ip3 ip4)
            (toWord64 ip5 ip6 ip7 ip8)


main :: IO ()
main =
    hspec $
    do
        describe "IPv6 to decimal" $ do
            it "can parse 0:0:0:0:0:ffff:cc78:f" $ do
                let p =
                        parseIPAddress6
                    i =
                        "0:0:0:0:0:ffff:cc78:f"
                    m =
                        parseString p mempty i
                    r' =
                        fmap toDecimal (maybeSuccess m)
                print m
                r' `shouldBe` Just 281474112159759
            it "can parse 0:0:0:0:0:ffff:ac10:fe01" $ do
                let p =
                        parseIPAddress6
                    i =
                        "0:0:0:0:0:ffff:ac10:fe01"
                    m =
                        parseString p mempty i
                    r' =
                        fmap toDecimal (maybeSuccess m)
                print m
                r' `shouldBe` Just 281473568538113
        describe "IPv6 parsing: " $ do
            it "can parse FE80:0000:0000:0000:0202:B3FF:FE1E:8329" $ do
                let p =
                        parseIPAddress6
                    i =
                        "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just
                    (IPAddress6 18338657682652659712 144876050090722089)
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
        describe "Shifting numbers:" $ do
            it "can calculate IPv6 decimal shifting and adding" $ do
                let high =
                        toWord64 65152 0 0 0
                    low =
                        toWord64 514 46079 65054 33577
                    total =
                        sumHighLow high low
                total `shouldBe` 338288524927261089654163772891438416681
                high `shouldBe` 18338657682652659712
                low `shouldBe` 144876050090722089
            it "can calculate lower IPv6 address number using toWord64" $ do
                let dec =
                        toWord64 0 65535 44048 65025
                dec `shouldBe` 281473568538113
            it "can calculate higher IPv6 address number" $ do
                let dec =
                        (shift 65152 48)
                        + (shift 0 32)
                        + (shift 0 16)
                        + 0 :: Integer
                dec `shouldBe` 18338657682652659712
            it "can calculate lower IPv6 address number" $ do
                let dec =
                        (shift 514 48)
                        + (shift 46079 32)
                        + (shift 65054 16)
                        + 33577 :: Integer
                dec `shouldBe` 144876050090722089
            it "can calculate an big IPv6 address by shifting and adding" $ do
                let dec =
                        (shift 65152 112)
                        + (shift 0 96)
                        + (shift 0 80)
                        + (shift 0 64)
                        + (shift 514 48)
                        + (shift 46079 32)
                        + (shift 65054 16)
                        + 33577 :: Integer
                dec `shouldBe` 338288524927261089654163772891438416681
            it "can calculate an IPv6 address by shifting and adding" $ do
                let dec =
                        (shift 0 112)
                        + (shift 0 96)
                        + (shift 0 80)
                        + (shift 0 64)
                        + (shift 0 48)
                        + (shift 65535 32)
                        + (shift 44048 16)
                        + 65025 :: Integer
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
