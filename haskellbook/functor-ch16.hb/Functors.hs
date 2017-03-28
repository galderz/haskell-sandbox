module Functors where


aMap =
    map (\x -> x > 3) [1..6]


anFmap =
    fmap (\x -> x > 3) [1..6]


-- map can't operate on anything other than lists
-- anInvalidMap =
--     map (+1) (Just 1)

-- fmap can operate on anything
fmapOnJust =
    fmap (+1) (Just 1)


fmapOnTuple =
    fmap (10/) (4, 5)


fmapOnEither =
    fmap (++ ", Esq.") (Right "Chris Allen")
