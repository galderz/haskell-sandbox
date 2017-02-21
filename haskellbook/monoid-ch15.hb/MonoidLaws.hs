module MonoidLaws where

import Data.Monoid


leftIdentity =
    mappend mempty (Sum 1) == (Sum 1)


rightIdentity =
    mappend (Sum 1) mempty == (Sum 1)
