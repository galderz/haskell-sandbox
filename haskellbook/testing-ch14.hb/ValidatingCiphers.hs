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


genCaesar :: Gen (String, Int)
genCaesar = do
    t <- genLowerString
    k <- arbitrary
    return (t, k)


prop_Caesar :: Property
prop_Caesar =
    forAll genCaesar
        (\(s, n) -> caesarIdentity s n)


main :: IO ()
main =
    do  quickCheck prop_Caesar
