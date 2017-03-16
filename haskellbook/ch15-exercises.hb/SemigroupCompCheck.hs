module SemigroupCompCheck where

import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck
import Test.QuickCheck.Function


newtype Comp a =
    Comp { unComp :: (a -> a) }


instance Semigroup (Comp a) where
    Comp f <> Comp g =
        Comp (g . f)


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


main :: IO ()
main =
    quickCheck $ \(Fun _ f) (Fun _ g) (Fun _ h) ->
        (compAssoc :: CompAssoc Int) (Comp f) (Comp g) (Comp h)
