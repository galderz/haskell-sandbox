module HaskellBook.Chapter10.WriteFolds where

import Data.Time

data DatabaseItem = 
  DbString String 
  | DbNumber Integer 
  | DbDate UTCTime
  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate 
      (UTCTime (fromGregorian 1911 5 1)        (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 20
    , DbString "Hello, world!"
    , DbDate 
      (UTCTime (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = 
  foldr appendUtc [] db
  where 
    appendUtc (DbDate utc) acc = 
        utc : acc
    appendUtc _ acc = 
        acc


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = 
  foldr appendDbNumber [] db
  where 
    appendDbNumber (DbNumber num) acc = 
        num : acc
    appendDbNumber _ acc = 
        acc


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = 
  foldr calcMostRecent earliestUTC (filterDbDate db)
  where earliestUTC = 
          (UTCTime (fromGregorian 1 1 1)            (secondsToDiffTime 34123))
        calcMostRecent utc acc
          | utc > acc = 
              utc
          | otherwise = 
              acc

sumDb :: [DatabaseItem] -> Integer
sumDb db = 
  foldr (+) 0 (filterDbNumber db)


avgDb :: [DatabaseItem] -> Double
avgDb db = 
  fromIntegral (sumDb db) / fromIntegral (length nums)
  where nums = 
          filterDbNumber db


