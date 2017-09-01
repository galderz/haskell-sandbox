module AltParsing where

import Control.Applicative
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
    (Left <$> integer)
    <|> (Right <$> some letter)


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
