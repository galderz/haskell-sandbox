import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Prelude hiding (sum, product)


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


main :: IO ()
main =
    do  print $ 5 == sum (7, 5)
        print $ [5, 4] == fmap sum [(7, 5), (3, 4)]
        print $ Just 15 == fmap sum (Just [1, 2, 3, 4, 5])
        print $ 1 == product Nothing
        print $ Just 1 == fmap product (Just [])
        print $ Right 6 == fmap product (Right [1, 2, 3] :: Either String [Int])
