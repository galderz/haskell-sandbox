module PrefixOrInfix where


-- Prefix, works.
data P =
    Prefix Int String


-- Infix, works
data Q =
    Int :!!: String


-- Since data constructor is symbolic rather than alphanumeric,
-- it can't be used as a prefix:
-- data R =
--     :!!: Int String


-- Alphanumeric data constructor can't be used as infix
-- data S =
--     Int Prefix String
