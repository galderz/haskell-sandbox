module UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)


half :: Fractional a => a -> a
half x =
    x / 2


halfIdentity :: Fractional a => a -> a
halfIdentity =
    (*2) . half


prop_FloatHalf :: Float -> Bool
prop_FloatHalf x =
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


prop_IntListSort :: [Int] -> Bool
prop_IntListSort = listOrdered . sort


plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z


prop_IntPlusAssociative :: Int -> Int -> Int -> Bool
prop_IntPlusAssociative = plusAssociative


plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
    x + y == y + x


prop_IntPlusCommutative :: Int -> Int -> Bool
prop_IntPlusCommutative = plusCommutative


productAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
productAssociative x y z =
    x * (y * z) == (x * y) * z


prop_IntProductAssociative :: Int -> Int -> Int -> Bool
prop_IntProductAssociative = productAssociative


productCommutative :: (Num a, Eq a) => a -> a -> Bool
productCommutative x y =
    x * y == y * x


prop_IntProductCommutative :: Int -> Int -> Bool
prop_IntProductCommutative = productCommutative


quotRemLaw :: Integral a => a -> a -> Bool
quotRemLaw x y =
    (quot x y) * y + (rem x y) == x


prop_IntQuotRemLaw :: Int -> NonZero Int -> Bool
prop_IntQuotRemLaw x (NonZero y) =
    quotRemLaw x y


divModLaw :: Integral a => a -> a -> Bool
divModLaw x y =
    div x y * y + mod x y == x


prop_IntDivModLaw :: Int -> NonZero Int -> Bool
prop_IntDivModLaw x (NonZero y) =
    divModLaw x y


powerAssociative :: (Num a, Integral b1, Integral b, Eq a) => a -> b1 -> b -> Bool
powerAssociative x y z =
    x ^ (y ^ z) == (x ^ y) ^ z


prop_IntPowerAssociative :: Int -> Int -> Int -> Bool
prop_IntPowerAssociative =
    powerAssociative


powerCommutative :: Integral b => b -> b -> Bool
powerCommutative x y =
    x ^ y == y ^ x


prop_IntPowerCommutative :: Int -> Int -> Bool
prop_IntPowerCommutative =
    powerCommutative


listReverse :: Eq a => [a] -> Bool
listReverse x =
    reverse (reverse x) == id x


prop_IntListReverse :: [Int] -> Bool
prop_IntListReverse =
    listReverse


main :: IO ()
main =
    do  quickCheck prop_FloatHalf
        quickCheck prop_IntListSort
        quickCheck prop_IntPlusAssociative
        quickCheck prop_IntPlusCommutative
        quickCheck prop_IntProductAssociative
        quickCheck prop_IntProductCommutative
        quickCheck prop_IntQuotRemLaw
        quickCheck prop_IntDivModLaw
        -- quickCheck prop_IntPowerAssociative
        -- quickCheck prop_IntPowerCommutative
        quickCheck prop_IntListReverse
