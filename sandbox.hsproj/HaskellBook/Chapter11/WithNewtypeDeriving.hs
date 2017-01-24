{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellBook.Chapter11.WithNewtypeDeriving where


class TooMany a where
    tooMany :: a -> Bool


instance TooMany Int where
    tooMany n = n > 42


newtype Goats =    Goats Int deriving (Eq, Show, TooMany)


-- We don't have to define an instance of TooMany for Goats
-- that's merely identical to the Int instance. We can reuse
-- the instance that we have.
-- This is nice for times when we want every typeclass instance
-- to be the same except for one we want to change.