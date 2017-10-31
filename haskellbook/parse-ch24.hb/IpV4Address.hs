import Data.Bits
import Data.Word
import Test.Hspec
import Text.Trifecta


data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord, Show)


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) =
    Just a
maybeSuccess _ =
    Nothing


dec1 :: Bits a => a -> a
dec1 n =
    shift n 24


dec2 :: Bits a => a -> a
dec2 n =
    shift n 16


dec3 :: Bits a => a -> a
dec3 n =
    shift n 8


toIp :: Integer -> Integer -> Integer -> Integer -> Word32
toIp i1 i2 i3 i4 =
    fromInteger $ (dec1 i1) + (dec2 i2) + (dec3 i3) + i4


parseIPAddress :: Parser IPAddress
parseIPAddress =
    do  ip1 <- decimal
        _ <- char '.'
        ip2 <- decimal
        _ <- char '.'
        ip3 <- decimal
        _ <- char '.'
        ip4 <- decimal
        return $ IPAddress $ toIp ip1 ip2 ip3 ip4


main :: IO ()
main =
    hspec $
    do
        describe "IPv4 parsing: " $ do
            it "can parse 172.16.254.1" $ do
                let p =
                        parseIPAddress
                    i =
                        "172.16.254.1"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                r' `shouldBe` Just (IPAddress 2886794753)
            it "can parse 204.120.0.15" $ do
                let p =
                        parseIPAddress
                    i =
                        "204.120.0.15"
                    m =
                        parseString p mempty i
                    r' =
                        maybeSuccess m
                r' `shouldBe` Just (IPAddress 3430416399)

        describe "Shifting numbers: " $ do
            it "can calculate an IP address by shifting and adding numbers" $ do
                let dec =
                        (dec1 172)
                        + (dec2 16)
                        + (dec3 254)
                        + 1 :: Word32
                dec `shouldBe` 2886794753
