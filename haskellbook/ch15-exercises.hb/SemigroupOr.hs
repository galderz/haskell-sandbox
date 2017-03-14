module SemigroupOr where

import Data.Semigroup
import Test.QuickCheck


data Or a b =
    Fst a
    | Snd b
    deriving (Eq, Show)


instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    Fst _ <> Snd b =
        Snd b

    Fst _ <> Fst a' =
        Fst a'

    Snd b <> Fst _ =
        Snd b

    Snd b <> Snd _ =
        Snd b


instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c))
        == ((a <> b) <> c)


type OrAssoc =
    Or String String -> Or String String -> Or String String -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: OrAssoc)
