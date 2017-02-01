module Addition where

import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"


divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum 0 _ =
  0
multiplyBySum _ 0 =
  0
multiplyBySum n 1 =
  n
multiplyBySum n x =
  n + (multiplyBySum n (x - 1))

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      divideBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 reminder 2" $ do
      divideBy 22 5 `shouldBe` (4, 2)
    it "5 times 2 is 10" $ do
      multiplyBySum 5 2 `shouldBe` 10
    it "0 times 2 is 0" $ do
      multiplyBySum 0 2 `shouldBe` 0
    it "2 times 0 is 0" $ do
      multiplyBySum 2 0 `shouldBe` 0
