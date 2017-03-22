module MonoidCombineCheck where

import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.QuickCheck
import Test.QuickCheck.Function


newtype Combine a b =
    Combine { unCombine :: (a -> b) }


instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine f' =
        Combine (f <> f')


instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty =
        Combine mempty

    mappend = (<>)


-- Test.QuickCheck.(===) requires (Eq b, Show b)
-- but you can use (==) if you prefer.
funEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
funEquality (Combine x) (Combine y) =
    property $ \a -> x a
        === y a


type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property


combineAssoc :: (Semigroup b, Arbitrary a, Show a, Eq b, Show b) => CombineAssoc a b
combineAssoc f g h =
    ((f <> g) <> h) `funEquality` (f <> (g <> h))


type CombineId a b = Combine a b -> Property


monoidLeftIdentity :: (Semigroup b, Monoid b, Arbitrary a, Show a, Eq b, Show b) => CombineId a b
monoidLeftIdentity f =
    (mempty <> f) `funEquality` f


monoidRightIdentity :: (Semigroup b, Monoid b, Arbitrary a, Show a, Eq b, Show b) => CombineId a b
monoidRightIdentity f =
    (f <> mempty) `funEquality` f


main :: IO ()
main =
    do  quickCheck $ \(Fun _ f) (Fun _ g) (Fun _ h) ->
            (combineAssoc :: CombineAssoc Int (Sum Int)) (Combine f) (Combine g) (Combine h)
        quickCheck $ \(Fun _ f) ->
            (monoidLeftIdentity :: CombineId Int (Sum Int)) (Combine f)
        quickCheck $ \(Fun _ f) ->
            (monoidRightIdentity :: CombineId Int (Sum Int)) (Combine f)
