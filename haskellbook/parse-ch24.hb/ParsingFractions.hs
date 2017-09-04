{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta


badFraction =
    "1/0"


alsoBad =
    "10"


shouldWork =
    "1/2"


shouldAlsoWork =
    "2/1"


parseFraction :: Parser Rational
parseFraction =
    do  numerator <- decimal
        char '/'
        denominator <- decimal
        return (numerator % denominator)


virtuousFraction :: Parser Rational
virtuousFraction =
    do  numerator <- decimal
        char '/'
        denominator <- decimal
        case denominator of
            0 -> fail "Denominator cannot be zero"
            _ -> return (numerator % denominator)


main :: IO ()
main =
    do  let parseFraction' =
                parseString parseFraction mempty
        print $ parseFraction' shouldWork
        print $ parseFraction' shouldAlsoWork
        print $ parseFraction' alsoBad
        print $ parseFraction' badFraction


testVirtuous :: IO ()
testVirtuous =
    do  let virtuousFraction' =
                parseString virtuousFraction mempty
        print $ virtuousFraction' badFraction
        print $ virtuousFraction' alsoBad
        print $ virtuousFraction' shouldWork
        print $ virtuousFraction' shouldAlsoWork


type FractionOrNumber =
    Either Rational Integer


parseFON :: Parser FractionOrNumber
parseFON =
    (Left <$> try parseFraction)
    <|> (Right <$> try integer)


testFractionOrNumber :: IO ()
testFractionOrNumber =
        do
            let p f i =
                    parseString f mempty i
            print $ p parseFON "blah"
            print $ p parseFON "123"
            print $ p parseFON shouldWork
            print $ p parseFON shouldAlsoWork
            print $ p parseFON badFraction
            print $ p parseFON alsoBad
