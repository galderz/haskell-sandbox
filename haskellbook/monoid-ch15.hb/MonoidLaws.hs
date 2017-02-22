module MonoidLaws where

import Data.Monoid


leftIdentity =
    mappend mempty (Sum 1) == (Sum 1)


rightIdentity =
    mappend (Sum 1) mempty == (Sum 1)


associativity =
    mappend (Sum 1) (mappend (Sum 2) (Sum 3)) ==
        mappend (mappend (Sum 1) (Sum 2)) (Sum 3)
