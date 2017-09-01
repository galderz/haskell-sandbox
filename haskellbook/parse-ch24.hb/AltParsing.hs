{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta


type NumberOrString =
    Either Integer String


a =
    "blah"


b =
    "123"


c =
    "123blah789"


parseNos :: Parser NumberOrString
parseNos =
    do
        skipMany (oneOf "\n")
        v <- (Left <$> integer)
            <|> (Right <$> some letter)
        skipMany (oneOf "\n")
        return v


parseNos' :: Parser NumberOrString
parseNos' =
    skipMany (oneOf "\n")
    >>
    (Left <$> integer)
    <|> (Right <$> some letter)


eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]


main :: IO ()
main =
    do  let p f i =
                parseString f mempty i
        print $ p (some letter) a
        print $ p integer b
        print $ p parseNos a
        print $ p parseNos b
        -- many: zero or more
        print $ p (many parseNos) c
        -- some: one or more
        print $ p (some parseNos) c
        print $ parseString (some integer) mempty "123"
        print $ parseString (some integer) mempty ""
        print $ parseString (many integer) mempty "123"
        print $ parseString (many integer) mempty ""
        print $ p parseNos eitherOr
        print $ p (some parseNos) eitherOr
        -- tokenize to get rid of \n at the end
        print $ p (some (token parseNos')) eitherOr
