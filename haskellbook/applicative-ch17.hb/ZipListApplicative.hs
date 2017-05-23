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


instance Functor List where
    fmap =
        undefined


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


main :: IO ()
main =
    do
        print $ drop' 2 (Cons 1 Nil)
        print $ drop' 2 (Cons 1 (Cons 2 (Cons 3 Nil)))
        print $ drop' 2 (Nil :: List String)
        print $ drop' (-1) (Cons 1 Nil)
        print $ drop' (0) (Cons 1 Nil)
        print $ take' 2 (Cons 1 Nil)
        print $ take' 2 (Cons 1 (Cons 2 (Cons 3 Nil)))
        print $ take' 2 (Nil :: List String)
        print $ take' (-1) (Cons 1 Nil)
        print $ take' (0) (Cons 1 Nil)
        --quickBatch $ functor (undefined :: List (String, String, Int))
