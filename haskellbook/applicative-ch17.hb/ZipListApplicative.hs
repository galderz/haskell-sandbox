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
    pure x =
        Cons x Nil
    (<*>) Nil _ =
        Nil
    (<*>) _ Nil =
        Nil
    (<*>) (Cons f xs) ca =
        mappend (fmap f ca) (xs <*> ca)


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


arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 =
    return Nil
arbList n =
    frequency [
        (1, return Nil)
        , (4, fmap Cons arbitrary <*> (arbList (n - 1)))
    ]


-- A great example of the applicative use:
-- When you find yourself wanting to do an fmap
-- on something that's contained within a container.
-- In this case, the container is Gen
instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary =
        pure ZipList' <*> sized arbList


l :: List (String, String, Int)
l =
    undefined :: List (String, String, Int)

main :: IO ()
main =
    do
        quickCheck (takeDrop :: Int -> List Int -> Bool)
        -- For confidence building, already tested by applicative
        -- quickBatch $ functor l
        -- quickBatch $ monoid l
        quickBatch $ applicative l
        quickBatch $ functor (undefined :: ZipList' (String, String, Int))
