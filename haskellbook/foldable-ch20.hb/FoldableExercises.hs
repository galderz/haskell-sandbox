data Constant a b =
    Constant a


instance Foldable (Constant a) where
    foldl f z (Constant x) =
        -- Can't be `f z x` because x is part of the container
        -- and hence it cannot be used.
        -- Only `b` is usable but that's not part of instance
        -- , so the only thing you can do is return z
        z

    foldr =
        undefined

    foldMap =
        undefined


main :: IO ()
main =
    do  print $ 5 == foldl (*) 5 (Constant 5)
