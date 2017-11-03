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


stringToInt :: String -> Integer
stringToInt s =
    toInteger $ foldl (\z x -> (z * 10) + digitToInt x) 0 s


base16Integer :: Parser Integer
base16Integer =
    fmap stringToInt (some parseHexDigit)


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) =
    Just a
maybeSuccess _ =
    Nothing


toWord64 :: Integer -> Integer -> Integer -> Integer -> Integer -> Word64
toWord64 padding i1 i2 i3 i4 =
    fromInteger $
        (shift (48 + padding) (fromInteger i1))
        + (shift (32 + padding) (fromInteger i2))
        + (shift (16 + padding) (fromInteger i3))
        + shift (0 + padding) (fromInteger i4)


toDecimal :: IPAddress6 -> Word64
toDecimal (IPAddress6 high low) =
    high + low


parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 =
    do  ip1 <- hexadecimal
        _ <- char ':'
        ip2 <- hexadecimal
        _ <- char ':'
        ip3 <- hexadecimal
        _ <- char '.'
        ip4 <- hexadecimal
        _ <- char ':'
        ip5 <- hexadecimal
        _ <- char ':'
        ip6 <- hexadecimal
        _ <- char ':'
        ip7 <- hexadecimal
        _ <- char ':'
        ip8 <- hexadecimal
        return $ IPAddress6
            (toWord64 0 ip1 ip2 ip3 ip4)
            (toWord64 48 ip5 ip6 ip7 ip8)


main :: IO ()
main =
    hspec $
    do
        -- describe "IPv6 parsing: " $ do
        --     it "can parse 0:0:0:0:0:ffff:ac10:fe01" $ do
        --         let p =
        --                 parseIPAddress6
        --             i =
        --                 "0:0:0:0:0:ffff:ac10:fe01"
        --             m =
        --                 parseString p mempty i
        --             r' =
        --                 maybeSuccess m

        --         r' `shouldBe` Just (IPAddress6 0 0)
        describe "Hexadecimal parsing: " $ do
            it "can parse f" $ do
                let p =
                        base16Integer
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
                        base16Integer
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
                        base16Integer
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
