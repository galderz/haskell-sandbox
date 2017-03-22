module MonoidCompCheck where

import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.QuickCheck
import Test.QuickCheck.Function


-- QuickCheck Solution suggested in:
-- http://stackoverflow.com/questions/41350192/how-to-test-semigroup-law-for-this-data-type


newtype Comp a =
    Comp { unComp :: (a -> a) }


instance Semigroup (Comp a) where
    Comp f <> Comp g =
        Comp (g . f)


instance Monoid (Comp a) where
    mempty =
        Comp id

    mappend = (<>)


-- Test.QuickCheck.(===) requires (Eq b, Show b)
-- but you can use (==) if you prefer.
funEquality :: (Arbitrary a, Show a, Eq a) => Comp a -> Comp a -> Property
funEquality (Comp f) (Comp g) =
    property $ \a -> f a
        === g a


type CompAssoc a = Comp a -> Comp a -> Comp a -> Property


compAssoc :: (Arbitrary a, Show a, Eq a) => CompAssoc a
compAssoc f g h =
    ((f <> g) <> h) `funEquality` (f <> (g <> h))


type CompId a = Comp a -> Property


monoidLeftIdentity :: (Arbitrary a, Show a, Eq a) => CompId a
monoidLeftIdentity f =
    (mempty <> f) `funEquality` f


monoidRightIdentity :: (Arbitrary a, Show a, Eq a) => CompId a
monoidRightIdentity f =
    (f <> mempty) `funEquality` f


main :: IO ()
main =
    do  quickCheck $ \(Fun _ f) (Fun _ g) (Fun _ h) ->
            (compAssoc :: CompAssoc Int) (Comp f) (Comp g) (Comp h)
        quickCheck $ \(Fun _ f) ->
            (monoidLeftIdentity :: CompId Int) (Comp f)
        quickCheck $ \(Fun _ f) ->
            (monoidRightIdentity :: CompId Int) (Comp f)
