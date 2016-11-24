module HaskellBook.Chapter11.ExercisesPityTheBool where

-- Exercises for Sum types
  

-- needed to have Int8 in scope
import Data.Int 


-- What's the cardinality of NumberOrBool?
-- 256 + 2 = 512
data NumberOrBool =
    Numba Int8    | BoolyBool Bool 
    deriving (Eq, Show)

-- What's the cardinality of this datatype?
-- 2 + 2 = 4
data BigSmall =
    Big Bool
    | Small Bool
    deriving (Eq, Show)
    
