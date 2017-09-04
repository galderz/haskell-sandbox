{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IniParsing where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta


headerEx :: ByteString
headerEx =
    "blah"


-- "[blah]" -> Section "blah"
newtype Header =
    Header String
    deriving (Eq, Ord, Show)


parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
    char '[' *> p <* char ']'
    -- these operators mean the brackets
    -- will be parsed and then discarded
    -- but the p will remain as our result


parseHeader :: Parser Header
parseHeader =
    parseBracketPair (Header <$> some letter)
