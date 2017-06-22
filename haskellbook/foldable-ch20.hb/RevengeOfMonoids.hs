import Data.Foldable
import Data.Monoid


main :: IO ()
main =
    do  print $ foldr (+) 0 [1..5]
        print $ fold [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]
