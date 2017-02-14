module Idempotence where

import Data.Char
import Test.QuickCheck


twice :: (b -> b) -> b -> b
twice f =
    f . f


fourTimes :: (b -> b) -> b -> b
fourTimes =
    twice . twice


capitalizeWord :: String -> String
capitalizeWord [] =
    ""
capitalizeWord (' ':xs) =
    " " ++ capitalizeWord xs
capitalizeWord (x:xs) =
    (toUpper x) : xs


idempotence :: String -> Bool
idempotence x =
    let
        capWord = capitalizeWord x
    in
        (capWord == twice capitalizeWord x)
            && (capWord == fourTimes capitalizeWord x)


main :: IO ()
main =
    quickCheck idempotence
