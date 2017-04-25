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
