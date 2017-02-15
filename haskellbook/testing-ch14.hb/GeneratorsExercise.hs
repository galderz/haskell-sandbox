module GeneratorsExercise where

import Test.QuickCheck


data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)


-- $ sample foolGen
foolGen :: Gen Fool
foolGen =
    oneof [return Fulse, return Frue]
