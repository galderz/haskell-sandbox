module HaskellBook.Chapter10.Exercises where

-- Functions named with ' are first attemtps
-- Functions with no ' are point-free versions

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f (x:[]) = 
    x
myMinimumBy' f (x:xs) = 
    foldl ord x xs
    where 
        ord z y
            | f z y == LT = 
                z
            | otherwise = 
                y


myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f (x:[]) = 
    x
myMaximumBy' f (x:xs) = 
    foldl ord x xs
    where 
        ord z y
            | f z y == GT = 
                z
            | otherwise = 
                y
        

squishAgain :: [[a]] -> [a]
squishAgain = 
    squishMap id


squishAgain' :: [[a]] -> [a]
squishAgain' xs = 
    squishMap (\x -> x) xs 


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = 
    foldr ((++) . f) []
    --foldr (\y z -> (f y) ++ z) [] xs


squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f xs = 
    foldr (\y z -> (f y) ++ z) [] xs


squish :: [[a]] -> [a]
squish = 
    foldr (++) []
    --foldr (\y -> (y ++)) []
    --foldr (\y z -> y ++ z) [] xs


squish' :: [[a]] -> [a]
squish' xs = 
    foldr (\y z -> y ++ z) [] xs


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = 
    foldr (\y z -> if' (f y) (y : z) z) []
    --foldr (\y z -> if (f y) then y : z else z) []


if' :: Bool -> a -> a -> a
if' True  x _ = 
    x
if' False _ y = 
    y


myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f xs = 
    foldr (\y z -> if (f y) then y : z else z) [] xs


myMap :: (a -> b) -> [a] -> [b]
myMap f = 
    foldr ((:) . f) []
    -- foldr (\y -> (f y :)) []
    -- foldr (\y z -> f y : z) [] xs


myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = 
    foldr (\y z -> f y : z) [] xs


myReverse :: [a] -> [a]
myReverse =
    foldl (flip (:)) []
    -- foldl (\z y -> y : z) []
    -- foldr (\y z -> z ++ [y]) [] xs


myReverse' :: [a] -> [a]
myReverse' xs =
    foldr (\y z -> z ++ [y]) [] xs


myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x =
    any ((==) x)


myElem :: Eq a => a -> [a] -> Bool
myElem x = 
    foldr ((||) . ((==) x)) False
    -- foldr (\y -> ((x == y) ||)) False
    -- foldr (\y z -> (x == y) || z) False


myElem' :: Eq a => a -> [a] -> Bool
myElem' x = 
    foldr (\y z -> (x == y) || z) False


myAny :: (a -> Bool) -> [a] -> Bool
myAny f = 
    foldr ((||) . f) False
    -- foldr (\a -> (f a ||)) False
    -- foldr (\a b -> f a || b) False


myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = 
    foldr (\a b -> f a || b) False


myOr :: [Bool] -> Bool
myOr = 
    foldr (||) False


-- fold, both myAnd and the folding function are point-free now
myAnd :: [Bool] -> Bool
myAnd =
    foldr (&&) True


-- fold, not point-free in the folding function
myAnd' :: [Bool] -> Bool
myAnd' = 
    foldr (\a b -> if a == False then False else b) 
          True


seekritFuncFract x = 
  fromIntegral (sum (map length (words x))) /
  fromIntegral (length (words x))


-- What does seekritFunc do?
-- Adds the length of each word in a sentence,
-- and divides it by the number of words in that sentence
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))


nouns = ["car", "house", "plane"]


verbs = ["drive", "rent", "fly"]


nounVerbCombo :: [(String, String, String)]
nounVerbCombo = 
  concat $ map (\s -> map (\(a, b) -> (a, b, s)) pairs) nouns
  where len = 
          length verbs
        gen = 
          map (replicate len) nouns
        pairs =
          concat $ map (\x -> zip x verbs) gen


stops  = "pbtdkg"


vowels = "aeiou"


letterComboStartP :: [(Char, Char, Char)]
letterComboStartP = 
  filter (\(a, b, c) -> a == 'p') letterCombo


letterCombo :: [(Char, Char, Char)]
letterCombo = 
  concat $ map (\s -> map (\(a, b) -> (a, b, s)) pairs) stops
  where len = 
          length vowels
        gen = 
          map (replicate len) stops
        pairs =
          concat $ map (\x -> zip x vowels) gen
