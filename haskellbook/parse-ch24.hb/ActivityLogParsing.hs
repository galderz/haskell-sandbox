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


skipComments :: Parser ()
skipComments =
    do   _ <- string "--"
         skipMany (noneOf "\n")
         skipEOL)


parseDate :: Parser Date
parseDate =
    undefined


maybeSuccess :: Result a -> Maybe a
maybeSuccess =
    undefined


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
