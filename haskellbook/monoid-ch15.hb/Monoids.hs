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


listAppendNumsMonoid :: Bool
listAppendNumsMonoid =
    mappend [1,2,3] [4,5,6]
        == (++) [1,2,3] [4,5,6]


listAppendStringMonoid =
    mappend "Trout" " goes well with garlic"
        == (++) "Trout" " goes well with garlic"


listConcatMonoid =
    mconcat [[1..3], [4..6]]
        == foldr (++) [] [[1..3], [4..6]]


listConcatMonoid2 =
    mconcat [[1..3], [4..6]]
        == foldr mappend mempty [[1..3], [4..6]]
