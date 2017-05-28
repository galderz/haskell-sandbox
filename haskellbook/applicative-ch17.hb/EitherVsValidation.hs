module EitherVsValidation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a =
    Failure' e
    | Success' a
    deriving (Eq, Show)


instance Functor (Validation e) where
    fmap _ (Failure' e) =
        Failure' e
    fmap f (Success' a) =
        Success' (f a)


instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            oneof [ return $ Failure' x
                    , return $ Success' y
                ]


instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) =
        eq


main :: IO ()
main =
    do  print (pure 1 :: Either String Int)
        print ((Right (+1) <*> Right 1) :: Either String Int)
        print $ Right (+1) <*> Left ":("
        print ((Left ":("  <*> Right 1) :: Either String Int)
        print ((Left ":(" <*> Left "sadface.png") :: Either String Int)
        quickBatch $ functor (undefined :: Validation (String, String, Int) (String, String, Int))
