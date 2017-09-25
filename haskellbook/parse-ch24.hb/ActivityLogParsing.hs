{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Data.ByteString (ByteString)
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


parseActivities :: Parser Activities
parseActivities =
    undefined


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

        describe "Day activities" $
            it "Can parse a single activity" $
            do
                let p =
                        skipComments >> parseDate >> parseActivities
                    i =
                        activityDay
                    m =
                        parseByteString p mempty i
                    r' =
                        maybeSuccess m
                print m
                r' `shouldBe` Just [(800, "Breakfast")]


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
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]
