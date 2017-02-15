module GeneratorsExercise where

import Test.QuickCheck


data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)


-- $ sample foolGenEqual
foolGenEqual :: Gen Fool
foolGenEqual =
    oneof [return Fulse, return Frue]


-- $ sample foolGenMoreFulse
foolGenMoreFulse :: Gen Fool
foolGenMoreFulse =
    frequency
        [ (2, return Fulse)
        , (1, return Frue)]
