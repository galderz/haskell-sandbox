{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellBook.Chapter11.ExercisesLogicGoats_1_2 where
  

-- Exercises for newtype


class TooMany a where
    tooMany :: a -> Bool


instance TooMany (Int, Int) where
    tooMany (n, n') = (n + n') > 42


instance TooMany (Int, String) where
    tooMany (n, s) = n > 42


instance TooMany Int where
    tooMany n = n > 42


newtype MultiGoats =    MultiGoats (Int, Int) deriving (Eq, Show, TooMany)


newtype Goats =    Goats Int deriving (Eq, Show, TooMany)
