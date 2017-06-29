import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Prelude hiding (elem, product, sum)


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
