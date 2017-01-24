module HaskellBook.Chapter08.DividedBy where
  
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom
  | num > 0 && denom > 0 = goPP num denom 0
  | num > 0 && denom < 0 = goPN num denom 0
  | num < 0 && denom < 0 = goNN num denom 0
  | num < 0 && denom > 0 = goNP num denom 0
  where goPP n   d count
         | n < d = (count, n)
         | otherwise = goPP (n - d) d (count + 1)
        goPN n   d count
         | n <= 0 = (count, n)
         | otherwise = goPN (n + d) d (count - 1)
        goNN n   d count
         | n > d = (count, n)
         | otherwise = goNN (n - d) d (count + 1)         
        goNP n   d count
         | n >= 0 = (count, n)
         | otherwise = goNP (n + d) d (count - 1)

data DividedResult = Result Integer| DividedByZero
  deriving Show

dividedBy' :: Integral a => a -> a -> (DividedResult, a)
dividedBy' num 0 = (DividedByZero, num)
dividedBy' num denom = (Result (fromIntegral (fst res)), snd res)
  where res = dividedBy num denom
         
--dividedBy :: Integral a => a -> a -> (a, a)
--dividedBy num denom = go num denom 0
--  where go n   d count
--         | n < d = (count, n)
--         | otherwise = go (n - d) d (count + 1)

--dividedBy :: Integral a => a -> a -> (a, a)
--dividedBy num denom
--  | num > 0 && denom > 0 = resPP
--  | num > 0 && denom < 0 = (negate (fst resPN), snd resPN)
--  | num < 0 && denom > 0 = (negate (fst resNP), snd resNP)
--  | num < 0 && denom < 0 = resNN
--  where resPP = go num denom 0
--        resPN = go num (negate denom) 0 
--        resNP = go (negate num) denom 0 
--        resNN = go (negate num) (negate denom) 0 
--        go n   d count
--         | n < d = (count, n)
--         | otherwise = go (n - d) d (count + 1)
