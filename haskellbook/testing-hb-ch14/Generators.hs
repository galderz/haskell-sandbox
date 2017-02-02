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


-- Tinker the odds
oneThroughThreeTinker :: Gen Int
oneThroughThreeTinker =
  elements [1, 2, 2, 2, 2, 3]


-- choose :: System.Random.Random a => (a, a) -> Gen a
-- elements :: [a] -> Gen a


genBool :: Gen Bool
genBool =
  choose (False, True)


genBool' :: Gen Bool
genBool' =
  elements [False, True]


genOrdering :: Gen Ordering
genOrdering =
  elements [LT, EQ, GT]


genChar :: Gen Char
genChar =
  elements ['a'..'z']
