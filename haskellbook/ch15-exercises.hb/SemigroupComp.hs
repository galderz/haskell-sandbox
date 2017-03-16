module SemigroupComp where


import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import Test.QuickCheck
import Test.QuickCheck.Function


newtype Comp a =
    Comp { unComp :: (a -> a) }


instance Semigroup (Comp a) where
    Comp f <> Comp g =
        Comp (g . f)


f :: Comp Integer
f =
    Comp $ \n -> n + 1


g :: Comp Integer
g =
    Comp $ \n -> n - 1


main :: IO ()
main =
    do  print $ unComp (f <> g) $ 0
        print $ unComp (f <> g) $ 1
        print $ unComp (f <> f) $ 1
        print $ unComp (g <> f) $ 1
