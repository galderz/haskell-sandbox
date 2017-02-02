module Generators where


import Test.QuickCheck

-- Execute from repl with either of this:
-- sample trivialInt
-- sample' trivialInt
trivialInt :: Gen Int
trivialInt = return 1


oneThroughThree :: Gen Int
oneThroughThree =
  elements [1, 2, 3]
