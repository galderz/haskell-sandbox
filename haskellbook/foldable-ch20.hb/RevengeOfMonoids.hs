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
        print $ foldMap All [True, False, True]
        print $ foldMap Any [(3 == 4), (9 > 5)]
        print $ foldMap First [Just 1, Nothing, Just 5]
        print $ foldMap Last [Just 1, Nothing, Just 5]
        print $ foldMap (*5) [1, 2, 3 :: Product Integer]
        print $ foldMap (*5) [1, 2, 3 :: Sum Integer]
        print $ foldr (*) 5 [1, 2, 3]
        -- The way foldr works, declaring a Monoid instance
        -- different from what is implied in the folding function
        -- doesn't change the result
        print $ foldr (*) 3 [1, 2, 3 :: Sum Integer]
        print $ foldr (*) 3 [1, 2, 3 :: Product Integer]
        -- With one fold element, the same applies to foldMap
        print (foldMap (*5) (Just 100) :: Product Integer)
        print (foldMap (*5) (Just 5) :: Sum Integer)
