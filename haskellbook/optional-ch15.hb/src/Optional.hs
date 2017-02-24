module Optional where

import Data.Monoid

data Optional a =
    Nada
    |Only a
    deriving (Eq, Show)


instance Monoid a => Monoid (Optional a) where
    mempty =
        undefined

    mappend x Nada =
        x

    mappend Nada x =
        x
