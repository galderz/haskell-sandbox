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


replaceWithP =
    const 'p' -- const :: a -> b -> a


rwpJust =
    fmap replaceWithP (Just 10) -- Just 'p'


rwpNothing =
    fmap replaceWithP Nothing -- Nothing


rwpIntList =
    fmap replaceWithP [1, 2, 3, 4, 5] -- "ppppp"


rwpString =
    fmap replaceWithP "Ave" -- "ppp"


rwpEmpty =
    fmap replaceWithP [] -- ""


rwpTupleInt =
    fmap replaceWithP (10, 20) -- "(10, 'p')"


rwpTupleString =
    fmap replaceWithP (10, "woo") -- "(10, 'p')"


tossEmOne =
    fmap (+1) negate


tossEmOne10 =
    tossEmOne 10 -- -9


tossEmOneMinus10 =
    tossEmOne (-10) -- 11


tossEmOne' =
    (+1) . negate -- tossEmOne' 10 // tossEmOne' (-10) => same results as above
