module AdditionQuickcheck where


import Test.QuickCheck


prop_additionGreater :: Int -> Bool
prop_additionGreater x =
  x + 1 > x


prop_additionGreaterLie :: Int -> Bool
prop_additionGreaterLie x =
  x + 0 > x


-- Using QuickCheck without Hspec
runQc :: IO ()
runQc = do
  () <- quickCheck prop_additionGreater
  quickCheck prop_additionGreaterLie
