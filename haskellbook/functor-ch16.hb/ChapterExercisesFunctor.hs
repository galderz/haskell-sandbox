module ChapterExercisesFunctor where

import GHC.Arr


-- Can a valid Functor be provided?

-- 1. No, kind *
data Bool =
   False | True


-- 2. Yes, kind * -> *
data BoolAndSomethingElse a =
    False' a | True' a


-- 3. Yes, kind * -> *
data BoolAndMaybeSomethingElse a =
    Falsish | Truish a


-- 4. Yes, kind * -> *
newtype Mu f =
    InF { outF :: f(Mu f) }


-- 5. No, kind is *
data D =
    D (Array Word Word) Int Int


data Sum b a =
    First a
    | Second b


instance Functor (Sum e) where
    fmap f (First a) =
        First (f a)
    fmap f (Second b) =
        Second b


data Company a c b =
    DeepBlue a c
    | Something b


instance Functor (Company e e') where
    fmap f (Something b) =
        Something (f b)
    fmap _ (DeepBlue a c) =
        DeepBlue a c


data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)


instance Functor (More x) where
    fmap f (L a b a') =
        L (f a) b (f a')
    fmap f (R b a b') =
        R b (f a) b'


main :: IO ()
main =
    do  print $ fmap (+1) (L 1 2 3) -- L 2 2 4
        print $ fmap (+1) (R 1 2 3) -- R 1 3 3
