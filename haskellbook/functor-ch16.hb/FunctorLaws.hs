module FunctorLaws where


idLaw =
    fmap id "Hi Julie" == id "Hi Julie"


compositionLaw =
    fmap ((+1) . (*2)) [1..5] == (fmap (+1) . fmap (*2) $ [1..5])


data WhoCares a =
    ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)


-- Law breaking Functor
instance Functor WhoCares where
    fmap _ ItDoesnt =
        WhatThisIsCalled

    fmap _ WhatThisIsCalled =
        ItDoesnt

    fmap f (Matter a) =
        Matter (f a)


-- Check how identity law is broken by functor above
idLawBreak :: Bool
idLawBreak =
    fmap id (ItDoesnt :: WhoCares String) == id (ItDoesnt :: WhoCares String)
    -- fmap id ItDoesnt == id ItDoesnt
