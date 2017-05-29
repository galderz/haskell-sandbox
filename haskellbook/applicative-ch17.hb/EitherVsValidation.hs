module EitherVsValidation where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp, TestBatch)
import Test.QuickCheck.Classes (applicative, functor, monoid)

data Validation e a =
    Failure e
    | Success a
    deriving (Eq, Show)


instance Functor (Validation e) where
    fmap _ (Failure e) =
        Failure e
    fmap f (Success a) =
        Success (f a)


-- This is different
instance Monoid e => Applicative (Validation e) where
    pure x =
        Success x
    (<*>) (Failure f) (Failure x) =
        Failure (mappend f x)
    (<*>) _ (Failure x) =
        Failure x
    (<*>) (Failure x) _ =
        Failure x
    (<*>) (Success f) (Success y) =
        Success (f y)


instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            oneof [ return $ Failure x
                    , return $ Success y
                ]


instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) =
        eq


v =
    undefined :: Validation (String, String, [Int]) (String, String, [Int])


main :: IO ()
main =
    do  print (pure 1 :: Either String Int)
        print ((Right (+1) <*> Right 1) :: Either String Int)
        print $ Right (+1) <*> Left ":("
        print ((Left ":("  <*> Right 1) :: Either String Int)
        print ((Left ":(" <*> Left "sadface.png") :: Either String Int)
        quickBatch $ functor v
        quickBatch $ applicative v
