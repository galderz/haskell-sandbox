import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Prelude hiding (elem, minimum, product, sum)


-- Nicer with foldMap than foldr
sum :: (Foldable t, Num a) => t a -> a
sum =
    getSum . foldMap Sum
    -- foldr (+) 0


-- Nicer with foldMap than foldr
product :: (Foldable t, Num a) => t a -> a
product =
    getProduct . foldMap Product
    -- foldr (*) 1


elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x =
    getAny . foldMap (\a -> Any (x == a))
    -- foldr (\a z -> (a == x) || z) False


minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs =
    foldr f Nothing xs
    -- foldr (\a z -> if ((Just a) < z) then (Just a) else z) Nothing xs
    where f a Nothing = Just a
          f a (Just x) = if (a < x) then Just a else Just x


main :: IO ()
main =
    do  print $ 5 == sum (7, 5)
        print $ [5, 4] == fmap sum [(7, 5), (3, 4)]
        print $ Just 15 == fmap sum (Just [1, 2, 3, 4, 5])
        print $ 1 == product Nothing
        print $ Just 1 == fmap product (Just [])
        print $ Right 6 == fmap product (Right [1, 2, 3] :: Either String [Int])
        print $ False == elem 2 (Just 3)
        print $ False == elem True (Left False)
        print $ False == elem True (Left True)
        print $ False == elem True (Right False)
        print $ True == elem True (Right True)
        print $ [False, False, True] == fmap (elem 3) [Right 1, Right 2, Right 3]
        print $ Just 1 < Just 2
        print $ Just 'e' == minimum "julie"
        print $ Just (Just 'e') == fmap minimum (Just "julie")
        print $ Just 'j' == minimum (Just 'j')
        print $ [Just 'j', Just 'u', Just 'l'] ==
            fmap minimum [Just 'j', Just 'u', Just 'l']
