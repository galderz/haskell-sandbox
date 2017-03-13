module Semigroups where

import Data.List.NonEmpty as N
import Data.Semigroup as S


xs =
    1 :| [2, 3]


ys =
    4 :| [5, 6]


zs =
    xs <> ys


zsHead =
    N.head zs


zsLength =
    N.length zs
