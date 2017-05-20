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
take' =
    undefined


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
