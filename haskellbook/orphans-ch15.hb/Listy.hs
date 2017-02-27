module Listy where


newtype Listy a =
    Listy [a]
    deriving (Eq, Show)


-- Avoid separating types and typeclass definitions.
--
-- If types are external, define newtypes and provide typeclasses
-- for those in same file.
--
-- instance Monoid (Listy a) where
--     mempty =
--         Listy []
--     mappend (Listy l) (Listy l') =
--         Listy $ mappend l l'
