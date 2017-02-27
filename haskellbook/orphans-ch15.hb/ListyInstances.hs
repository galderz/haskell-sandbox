module ListyInstances where

import Data.Monoid
import Listy


instance Monoid (Listy a) where
    mempty =
        Listy []
    mappend (Listy l) (Listy l') =
        Listy $ mappend l l'

-- Build instructions:
-- $ stack ghc -- -I. --make ListyInstances.hs
