module ListApplicative where

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


main :: IO ()
main =
    quickBatch $ functor (undefined :: List (String, String, Int))
    -- quickBatch $ functor (Cons 1 Nil)
