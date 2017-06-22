import Data.Foldable
import Data.Monoid


main :: IO ()
main =
    do  print $ foldr (+) 0 [1..5]
        print $ fold [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]
        print $ fold [1, 2, 3, 4, 5 :: Sum Integer]
        print $ fold [1, 2, 3, 4, 5 :: Product Integer]
        print $ foldr (++) "" ["hello", "julie"]
        print $ fold ["hello", "julie"]
        print $ foldMap Sum [1, 2, 3, 4]
        print $ foldMap Product [1, 2, 3, 4]
