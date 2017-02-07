module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck


allowedChars :: [Char]
allowedChars =
    M.keys letterToMorse


allowedMorse :: [Morse]
allowedMorse =
    M.elems letterToMorse


charGen :: Gen Char
charGen =
    elements allowedChars


morseGen :: Gen Morse
morseGen =
    elements allowedMorse


-- with infix notation
prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c)
        >>= morseToChar) == Just c)


-- with do notation
prop_thereAndBackAgain' :: Property
prop_thereAndBackAgain' =
    forAll charGen
    (\c ->
        (do m <- charToMorse c
            morseToChar m)
            == Just c)


-- stack build && stack ghci morse:tests
main :: IO()
main =
    quickCheck prop_thereAndBackAgain'
--    quickCheck prop_thereAndBackAgain
