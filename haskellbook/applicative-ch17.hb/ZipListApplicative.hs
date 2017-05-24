module ZipListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)


take' :: Int -> List a -> List a
take' _ Nil =
    Nil
take' n (Cons a xs)
    | n <= 0 =
        Nil
    | otherwise =
        Cons a (take' (n - 1) xs)


drop' :: Int -> List a -> List a
drop' _ Nil =
    Nil
drop' n l@(Cons _ xs)
    | n <= 0 =
        l
    | otherwise =
        drop' (n - 1) xs


append :: List a -> List a -> List a
append Nil ys =
    ys
append (Cons x xs) ys =
    Cons x $ xs `append` ys


takeDrop :: Eq a => Int -> List a -> Bool
takeDrop n l =
    append (take' n l) (drop' n l) == l


instance Eq a => EqProp (List a) where
    (=-=) =
        eq


instance Functor List where
    fmap _ Nil =
        Nil
    fmap f (Cons x xs) =
        Cons (f x) (fmap f xs)


instance Monoid (List a) where
    mempty =
        Nil
    mappend xs Nil =
        xs
    mappend Nil xs =
        xs
    mappend (Cons x xs) cy =
        Cons x (mappend xs cy)


instance Applicative List where
    pure =
        undefined
    (<*>) =
        undefined


newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)


instance Eq a => EqProp (ZipList' a) where
    xs =-= ys =
        xs' `eq` ys'
        where   xs' =
                    let (ZipList' l) = xs in take' 3000 l
                ys' =
                    let (ZipList' l) = ys in take' 3000 l


instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs


instance Applicative ZipList' where
    pure =
        undefined
    (<*>) =
        undefined


instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        sized arbList


-- Not yet know what "<*>" does exactly, but it's neat :)
arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 =
    return Nil
arbList n =
    frequency [
        (1, return Nil)
        , (4, fmap Cons arbitrary <*> (arbList (n - 1)))
    ]


main :: IO ()
main =
    do
        quickCheck (takeDrop :: Int -> List Int -> Bool)
        quickBatch $ functor (undefined :: List (String, String, Int))
        quickBatch $ monoid (undefined :: List (String, String, Int))
