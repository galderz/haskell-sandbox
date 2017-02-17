module Monoids where


-- basic monoid rule
basicMonoidRule =
    mappend [1..5] [] ==
        mappend [] [1..5]
