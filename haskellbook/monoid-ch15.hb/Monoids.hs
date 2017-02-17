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


listAppendMonoid :: Bool
listAppendMonoid =
    mappend [1,2,3] [4,5,6]
        == (++) [1,2,3] [4,5,6]
