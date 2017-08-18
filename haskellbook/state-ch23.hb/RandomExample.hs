module RandomExample where


import System.Random


-- Six-sided die
data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)


intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        -- Use this tactic _extremely_ sparingly.
        x -> error $ "intToDie got non 1-6 integer: " ++ show x
