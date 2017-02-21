module MonoidLaws where

import Data.Monoid


leftIdentity =
    mappend mempty (Sum 1) == (Sum 1)
