import Data.Foldable
import Data.Monoid


main :: IO ()
main =
    do  print $ toList (Just 1)
        print $ map toList [Just 1, Just 2, Just 3]
