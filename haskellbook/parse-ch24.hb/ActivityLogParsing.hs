{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (digitToInt)
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta


-- aka area code
type Date =
    String


type Activities =
    [Activity]


type Activity =
    (Time, Description)


type Time =
    Int


type Description =
    String


type History =
    [(Date, Activities)]


data Log =
    Log History
    deriving (Eq, Show)


skipWhitespace :: Parser ()
skipWhitespace =
    skipMany (char ' ' <|> char '\n')


-- | Skip end of line and
--   whitespace beyond.
skipEOL :: Parser ()
skipEOL =
    skipMany (oneOf "\n")


skipComments :: Parser ()
skipComments =
    skipMany (
    do  _ <- string "--"
        skipMany (noneOf "\n")
        skipEOL)


parseDate :: Parser Date
parseDate =
    do  _ <- char '#'
        skipMany (oneOf " ")
        date <- some (noneOf "\n")
        skipEOL -- important!
        return date


stringToInt :: String -> Int
stringToInt s =
    foldl (\z x -> (z * 10) + digitToInt x) 0 s


parseTime :: Parser Time
parseTime =
    do  h1 <- digit
        h2 <- digit
        _ <- char ':'
        m1 <- digit
        m2 <- digit
        return $ (100 * stringToInt (h1 : h2 : []))
            + (stringToInt (m1 : m2 : []))


parseActivity :: Parser Activity
parseActivity =
    do  time <- parseTime
        _ <- char ' '
        desc <- some (noneOf "\n")
        skipEOL -- important!
        return (time, desc)


parseActivities :: Parser Activities
parseActivities =
    do  activities <- some parseActivity
        return activities


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) =
    Just a
maybeSuccess _ =
    Nothing


main :: IO ()
main =
    hspec $
    do
        describe "Comment parsing:" $ do
            it "skips comment before date" $ do
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

        describe "Day activities:" $ do
            it "can parse a single activity" $ do
                let p =
                        parseActivity
                    i =
                        "08:00 Breakfast"
                    m =
                        parseByteString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just (800, "Breakfast")

            it "can parse a day followed by a single activity" $ do
                let p =
                        skipWhitespace >> skipComments >> parseDate
                        >> parseActivity
                    i =
                        activityDay
                    m =
                        parseByteString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just (800, "Breakfast")

            it "can parse a day followed by a multiple activities" $ do
                let p =
                        skipWhitespace >> skipComments >> parseDate
                        >> parseActivities
                    i =
                        activitiesDay
                    m =
                        parseByteString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just [
                    (800, "Breakfast")
                    , (900, "Sanitizing moisture collector")
                    , (1100, "Exercising in high-grav gym")
                    , (1730, "R&R")
                    ]


activityDay :: ByteString
activityDay = [r|
# 2025-02-05
08:00 Breakfast
|]


activitiesDay :: ByteString
activitiesDay = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
17:30 R&R
|]
