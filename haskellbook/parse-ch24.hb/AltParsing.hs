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
