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


data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)


instance Functor CountingBad where
    fmap f (Heisenberg n a) =
        Heisenberg (n+1) (f a)


oneWhoKnocks =
    Heisenberg 0 "Uncle"


idLawBreakCountingBad =
    -- Heisenberg 2 "Uncle lol Jesse"
    (fmap (++ " Jesse") . fmap (++ " lol") $ oneWhoKnocks) ==
        (fmap ((++ " Jesse") . (++ " lol")) $ oneWhoKnocks)
        -- Heisenberg 1 "Uncle lol Jesse"


data CountingGood a =
    HeisenbergGood Int a
    deriving (Eq, Show)


instance Functor CountingGood where
    fmap f (HeisenbergGood n a) =
        HeisenbergGood (n) (f a)
