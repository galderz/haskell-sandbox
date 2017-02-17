module Monoids where


-- basic monoid rule
basicMonoidRule :: Bool
basicMonoidRule =
    mappend [1..5] []
        == mappend [] [1..5]


basicEmptyMonoidRule :: (Monoid a, Eq a) => a -> Bool
basicEmptyMonoidRule x =
    mappend x mempty
        == mappend mempty x
