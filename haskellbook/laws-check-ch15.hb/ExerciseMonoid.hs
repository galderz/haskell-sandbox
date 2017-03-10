module ExerciseMonoid where

import Data.Monoid
import Optional
import Test.QuickCheck


newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)


firstGen :: Arbitrary a => Gen (First' a)
firstGen =
    do  a <- arbitrary
        oneof   [ return $ First' (Only a)
                , return $ First' Nada ]


instance Arbitrary a => Arbitrary (First' a) where
    arbitrary =
        firstGen

-- instance Arbitrary Bull where
--     arbitrary =
--         frequency   [ (1, return Fools)
--                     , (1, return Twoo)]


instance Monoid (First' a) where
    mempty =
        First' Nada

    mappend x (First' Nada) =
        x

    mappend (First' Nada) x =
        x

    mappend (First' (Only x)) _ =
        First' (Only x)


firstMappend :: First' a -> First' a -> First' a
firstMappend =
    mappend


type FirstMappend =
    First' String
    -> First' String
    -> First' String
    -> Bool


type FstId =
    First' String -> Bool


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c))
        == ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
  (mempty <> a)
        == a


-- Test with:
-- quickCheck (monoidRightIdentity :: String -> Bool)
monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (a <> mempty)
        == a


main :: IO ()
main =
    do  quickCheck (monoidAssoc :: FirstMappend)
        quickCheck (monoidLeftIdentity :: FstId)
        quickCheck (monoidRightIdentity :: FstId)
