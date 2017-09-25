{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Data.ByteString()
import Test.Hspec
import Text.Trifecta


-- aka area code
type Date =
    String


type Activities =
    [Activity]


type Activity =
    (Time, Description)


type Time =
    Integer


type Description =
    String


type History =
    [(Date, Activities)]


data Log =
    Log History
    deriving (Eq, Show)


-- | Skip end of line and
--   whitespace beyond.
skipEOL :: Parser ()
skipEOL =
    skipMany (oneOf "\n")


skipComments :: Parser ()
skipComments =
    do  _ <- string "--"
        skipMany (noneOf "\n")
        skipEOL


parseDate :: Parser Date
parseDate =
    do  _ <- char '#'
        skipMany (oneOf " ")
        date <- some anyChar
        return date


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) =
    Just a
maybeSuccess _ =
    Nothing


main :: IO ()
main =
    hspec $
    do
        describe "Comment parsing" $
            it "Skips comment before date" $
            do
                let p =
                        skipComments >> parseDate
                    i =
                        "-- wheee a comment\n\n# 2025-02-05"
                    m =
                        parseByteString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just "2025-02-05"
