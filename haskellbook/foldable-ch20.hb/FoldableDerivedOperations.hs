import Data.Foldable
import Data.Monoid


main :: IO ()
main =
    do  print $ toList (Just 1)
        print $ map toList [Just 1, Just 2, Just 3]
        print $ concatMap toList [Just 1, Just 2, Just 3]
        print $ concatMap toList [Just 1, Just 2, Nothing]
        print $ toList (1, 2)
        print $ null (Left 3)
        print $ null []
        print $ null Nothing
        print $ null (1, 2)
        print $ fmap null [Just 1, Just 2, Nothing]
        print $ length (1, 2)
        print $ length [(1, 2), (3, 4), (5, 6)]
        print $ fmap length [(1, 2), (3, 4), (5, 6)]
        print $ fmap length Just [1, 2, 3]
        print $ fmap length [Just 1, Just 2, Just 3]
        print $ fmap length [Just 1, Just 2, Nothing]
