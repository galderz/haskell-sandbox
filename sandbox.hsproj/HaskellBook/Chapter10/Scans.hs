module HaskellBook.Chapter10.Scans where

fibs = 
    1 : scanl (+) 1 fibs


fibsN x =
    fibs !! x


fibs20 =
    take 20 fibs


fibsLess100 = 
    takeWhile (< 100) fibs
--    takeWhile (\x -> x < 100) fibs


factorial :: Integer -> Integer
factorial 0 = 
    1
factorial n = 
    n * factorial (n - 1)


factorialScanN :: Integer -> Integer
factorialScanN n = 
    last (scanl (*) 1 [1..n])


-- incorrect implementation
factorialScan' = 
    scanl (*) 1 factorialScan'

-- correct implementation
factorialScan'' = 
    scanl (*) 1 [1..]

-- http://chrisdone.com/posts/twitter-problem-loeb
water :: [Int] -> Int
water h =
    sum (
        zipWith (-)
            (zipWith min (scanl1 max h) (scanr1 max h))
               h)