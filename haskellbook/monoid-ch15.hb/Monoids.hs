module Monoids where

import Data.Monoid


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


-- No monoid exists for numbers because
-- it could either be a sum or product
-- since both are monoidal, but there can
-- only be one unique instance for a given
-- type class
-- integerMonoid =
--     mappend 1 1


-- Conflict resolved using Sum and Product
-- newtypes to wrap numeric values
sumMonoid =
    mappend (Sum 1) (Sum 5)


productMonoid =
    mappend (Product 5) (Product 5)


floatSumMonoid =
    mappend (Sum 4.5) (Sum 3.4)


-- Does not work because String is not a Num
-- stringMonoid =
--     Sum "Frank" <> Sum " " <> Sum "Herbert"

nestedSumMonoidVerbose =
    mappend (Sum 1) (mappend (Sum 2) (Sum 3))


nestedSumMonoid =
     (Sum 1) <> (Sum 1) <> (Sum 1)


nestedSumMonoid2 =
    (Sum 1) `mappend` (Sum 1) `mappend` (Sum 1)


sumRecordAccessor =
    getSum $ mappend (Sum 1) (Sum 1)


productRecordAccessor =
    getProduct $ mappend (Product 5) (Product 5)


sumRecordAccessorConcat =
    getSum $ mconcat [(Sum 5), (Sum 6), (Sum 7)]


productMonoidFoldr =
    foldr mappend mempty ([2, 4, 6] :: [Product Int])


sumMonoidFoldr =
    foldr mappend mempty ([2, 4, 6] :: [Sum Int])


stringMonoidFoldr =
    foldr mappend mempty ["blah", "woot"]


conjuctionBoolTrue =
    All True <> All True ==
        All True


conjuctionBoolFalse =
    All True <> All False ==
        All False
