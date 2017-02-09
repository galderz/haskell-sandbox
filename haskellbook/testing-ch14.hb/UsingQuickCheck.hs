module UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)


half :: Fractional a => a -> a
half x =
    x / 2


halfIdentity :: Fractional a => a -> a
halfIdentity =
    (*2) . half


prop_half :: Float -> Bool
prop_half x =
    halfIdentity x == x


-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where   go _ status@(_, False) =
                status
            go y (Nothing, t) =
                (Just y, t)
            go y (Just x, _) =
                (Just y, x >= y)


prop_listSort :: [Int] -> Bool
prop_listSort = listOrdered . sort


main :: IO ()
main =
    do  quickCheck prop_half
        quickCheck prop_listSort
