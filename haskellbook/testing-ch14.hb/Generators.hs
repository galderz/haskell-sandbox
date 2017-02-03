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

-- sample genTuple
-- sample (genTuple :: Gen (Int, Float))
-- sample (genTuple :: Gen ([()], Char))
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)


genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

-- sample (genEither :: Gen (Either Int Float))
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- sample (genMaybe :: Gen (Maybe String))
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]


-- What QuickCheck actually does
-- so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
