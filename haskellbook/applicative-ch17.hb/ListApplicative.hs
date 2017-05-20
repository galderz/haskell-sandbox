module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)


instance Functor List where
    fmap _ Nil =
        Nil
    fmap f (Cons a l) =
        Cons (f a) (fmap f l)


instance Monoid (List a) where
    mempty =
        Nil
    mappend l Nil =
        l
    mappend Nil l =
        l
    mappend (Cons a l) ys =
        Cons a (mappend l ys)


instance Applicative List where
    pure x =
        Cons x Nil
    (<*>) _ Nil =
        Nil
    (<*>) Nil _ =
        Nil
    (<*>) (Cons f l) ca =
        -- Cons (f a) (l <*> l')
        -- Cons (f a) ((Cons f l) <*> l')
        mappend (fmap f ca) (l <*> ca)


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


instance Eq a => EqProp (List a) where
    (=-=) =
        eq


-- Test functions/values
functions =
    Cons (+1) (Cons (*2) Nil)


values =
    Cons 1 (Cons 2 Nil)


append :: List a -> List a -> List a
append Nil ys =
    ys
append (Cons x xs) ys = Cons x $ xs `append` ys


fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil =
    b
fold f b (Cons h t) =
    f h (fold f b t)


concat' :: List (List a) -> List a
concat' = fold append Nil


-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as =
    concat' (fmap f as)


c =
    Cons


xs =
    foldr Cons Nil [1,2,3]


main :: IO ()
main =
    do  print $ mappend (Cons 1 Nil) (Cons 2 Nil)
        print $ (Cons (+1) Nil) <*> Cons 1 (Cons 2 Nil)
        print $ (Cons (*2) Nil) <*> Cons 1 (Cons 2 Nil)
        print $ (Cons (+1) (Cons (*2) Nil)) <*> Cons 1 (Cons 2 Nil)
        quickBatch $ applicative (undefined :: List (String, String, Int))
        -- quickBatch $ functor (undefined :: List (String, String, Int))
