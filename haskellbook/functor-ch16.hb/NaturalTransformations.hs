{-# LANGUAGE RankNTypes #-}
-- In REPL:
-- :set -XRank2Types

module NaturalTransformations where


type Nat f g = forall a. f a -> g a


maybeToList :: Nat Maybe []
maybeToList Nothing =
    []
maybeToList (Just a) =
    [a]


-- Won't work, not allowed
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing =
--     []
-- degenerateMtl (Just a) =
--     [a + 1]

type BadNat f g a = f a -> g a


maybeToList' :: BadNat Maybe [] a
maybeToList' Nothing =
    []
maybeToList' (Just a) =
    [a]


-- But this will too if we tell it
-- 'a' is Num a => a
-- However, it should not work!
degenerateMtl :: Num a => BadNat Maybe [] a
degenerateMtl Nothing =
    []
degenerateMtl (Just a) =
    [a + 1]
