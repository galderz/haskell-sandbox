import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Prelude hiding (sum)


-- Nicer with foldMap than foldr
sum :: (Foldable t, Num a) => t a -> a
sum =
    getSum . foldMap Sum
    -- foldr (+) 0


main :: IO ()
main =
    do  print $ 5 == sum (7, 5)
        print $ [5, 4] == fmap sum [(7, 5), (3, 4)]
        print $ Just 15 == fmap sum (Just [1, 2, 3, 4, 5])
