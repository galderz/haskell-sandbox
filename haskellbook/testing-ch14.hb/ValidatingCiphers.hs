module ValidatingCiphers where

import Data.Char
import Test.QuickCheck

caesar :: String -> Int -> String
caesar "" _ =
    ""
caesar (x:xs) n =
    chr (i + chAt) : (caesar xs n)
    where chAt = mod ((ord x) + n - i) 26
          i = ordFst x
          ordFst ch
            | isUpper ch = 65
            | otherwise = 97


unCaesar :: String -> Int -> String
unCaesar s n =
    caesar s (negate n)


caesarIdentity :: String -> Int -> Bool
caesarIdentity s n =
    unCaesar (caesar s n) n == s


genLowerAlpha :: Gen Char
genLowerAlpha =
  elements ['a'..'z']


genLowerString :: Gen String
genLowerString =
    listOf genLowerAlpha


genCaesarLower :: Gen (String, Int)
genCaesarLower = do
    t <- genLowerString
    k <- arbitrary
    return (t, k)


prop_CaesarLower :: Property
prop_CaesarLower =
    forAll genCaesarLower
        (\(s, n) -> caesarIdentity s n)


genUpperAlpha :: Gen Char
genUpperAlpha =
  elements ['A'..'Z']


genUpperString :: Gen String
genUpperString =
    listOf genUpperAlpha


genCaesarUpper :: Gen (String, Int)
genCaesarUpper = do
    t <- genUpperString
    k <- arbitrary
    return (t, k)


prop_CaesarUpper :: Property
prop_CaesarUpper =
    forAll genCaesarUpper
        (\(s, n) -> caesarIdentity s n)


main :: IO ()
main =
    do  quickCheck prop_CaesarLower
        quickCheck prop_CaesarUpper
