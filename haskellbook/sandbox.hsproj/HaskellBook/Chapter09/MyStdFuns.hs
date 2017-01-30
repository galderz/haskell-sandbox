module HaskellBook.Chapter09.MyStdFuns where


myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
--myMaximum xs = myMaximumBy compare xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = 
    x
myMinimumBy f (x:xs) = 
    rec f x xs
    where 
        rec f' acc (y:[])
              | f' acc y == LT = 
                  acc
              | otherwise = 
                  y
        rec f' acc (y:ys)
              | f' acc y == LT = 
                  rec f' acc ys
              | otherwise      = 
                  rec f' y ys


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = 
    x
myMaximumBy f (x:xs) = 
    rec f x xs
    where 
        rec f' acc (y:[])
              | f' acc y == GT = 
                  acc
              | otherwise = 
                  y
        rec f' acc (y:ys)
              | f' acc y == GT = 
                  rec f' acc ys
              | otherwise      = 
                  rec f' y ys


squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)
--squishAgain xs = squishMap (\x -> x) xs


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = 
    []
squishMap f (x:xs) = 
    (f x) ++ squishMap f xs


squish :: [[a]] -> [a]
squish [] = 
    []
squish (x:xs) = 
    x ++ squish xs


myReverse :: [a] -> [a]
myReverse [] = 
    []
myReverse (x:xs) = 
    myReverse xs ++ [x]


myElemAny :: Eq a => a -> [a] -> Bool
myElemAny n xs = 
    any (\x -> x == n) xs


myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = 
    False
myElem n (x:xs) =
    n == x || myElem n xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = 
    False
myAny f (x:xs) =
    f x || myAny f xs


myOr :: [Bool] -> Bool
myOr [] = 
    False
myOr (x:xs) = 
    x || myOr xs

