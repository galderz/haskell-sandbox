module Combinations where

import Control.Applicative (liftA3)


stops :: String
stops =
    "pbtdkg"


vowels :: String
vowels =
    "aeiou"


combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs =
    liftA3 (\x y z -> (x, y, z)) as bs cs
