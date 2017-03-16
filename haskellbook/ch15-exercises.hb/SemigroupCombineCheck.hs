module SemigroupCombineCheck where

import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.QuickCheck
import Test.QuickCheck.Function

newtype Combine a b =
    Combine { unCombine :: (a -> b) }


instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine f' =
        Combine (f <> f')


-- Test.QuickCheck.(===) requires (Eq b, Show b)
-- but you can use (==) if you prefer.
funEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEquality (Combine f) (Combine g) =
    property $ \a -> f a
        === g a


type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property


combineAssoc :: (Semigroup b, Arbitrary a, Show a, Eq b, Show b) => CombineAssoc a b
combineAssoc f g h =
    ((f <> g) <> h) `funEquality` (f <> (g <> h))


-- Book says:
-- Remember that the type of the value inside Combine is that of a function.
-- If you can't figure CoArbitrary...
-- Clarification:
-- The hint is not suggesting making a CoArbitrary (Combine a b),
-- but rather using a CoArbitrary a instance to implement Arbitrary (Combine a b).
-- I implicitly use CoArbitrary a (on the argument type) when generating
-- functions with Fun which is provided by the QuickCheck library,
-- the hint is about how you can implement such a generator of functions.


main :: IO ()
main =
    quickCheck $ \(Fun _ f) (Fun _ g) (Fun _ h) ->
        (combineAssoc :: CombineAssoc Int (Sum Int)) (Combine f) (Combine g) (Combine h)
