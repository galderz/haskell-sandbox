{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AesonMarshalling where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ


-- To test from ghci:
-- decode sectionJson :: Maybe Value
sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"} }
|]
