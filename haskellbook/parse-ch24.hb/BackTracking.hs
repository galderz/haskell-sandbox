{-# LANGUAGE OverloadedStrings #-}

module BT where


import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)


-- Helper function to run a trifecta parser and print the result
trifP :: Show a => Parser a -> String -> IO ()
trifP p i =
    print $ parseString p mempty i


-- Helper function to run a parsec parser and print the result
parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP =
    parseTest


-- Helper function to run a attoparsec parser and print the result
attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i =
    print $ parseOnly p i


nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse =
    (char '1' >> char '2') <|> char '3'


-- Backtracks if the first parse fails
tryParse :: (Monad f, CharParsing f) => f Char
tryParse =
    try (char '1' >> char '2')
    <|> char '3'


main :: IO ()
main =
    do
        -- trifecta
        trifP nobackParse "13"
        trifP tryParse "13"
        -- parsec
        parsecP nobackParse "13"
        parsecP tryParse "13"
        -- attoparsec
        attoP nobackParse "13"
        attoP tryParse "13"
