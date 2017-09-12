{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AesonMarshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ


-- To test from ghci:
-- decode sectionJson :: Maybe Value
sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"} }
|]


data TestData =
    TestData {
    section :: Host
    , what :: Color
    } deriving (Eq, Show)


newtype Host =
    Host String
    deriving (Eq, Show)


type Annotation = String


data Color =
    Red Annotation
    | Blue Annotation
    | Yellow Annotation
    deriving (Eq, Show)



instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section" <*> v .: "whatisit"

    parseJSON _ =
        fail "Expected an object for TestData"


instance FromJSON Host where
    parseJSON (Object v) =
        Host <$> v .: "host"

    parseJSON _ =
        fail "Expected an object for Host"


instance FromJSON Color where
    parseJSON (Object v) =
        (Red <$> v .: "red")
        <|> (Blue <$> v .: "blue")
        <|> (Yellow <$> v .: "yellow")

    parseJSON _ =
        fail "Expected an object for Color"


-- Testing out from ghci:
-- :set -XOverloadedStrings
-- decode "{\"blue\": \"123\"}" :: Maybe Color
-- Just (Blue "123")
-- :set -XQuasiQuotes
-- decode [r|{"red": "123"}|] :: Maybe Color
-- Just (Red "123")


main =
    do
        let d :: Maybe TestData
            d = decode sectionJson
        print d
