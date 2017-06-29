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
        print $ elem 2 (Just 3)
        print $ elem True (Left False)
        print $ elem True (Left True)
        print $ elem True (Right False)
        print $ elem True (Right True)
        print $ fmap (elem 3) [Right 1, Right 2, Right 3]
        print $ maximum [10, 12, 33, 5]
        -- Left and Nothing (and similar) values are empty
        -- for the purpouses of these functions
        print $ maximum [Just 2, Just 10, Just 4]
        print $ fmap maximum (Just [3, 7, 10, 2])
        print $ minimum "julie"
        print $ fmap minimum (Just "julie")
        print $ minimum (Just 'j')
        print $ fmap minimum [Just 'j', Just 'u', Just 'l']
        -- print $ fmap minimum [Just 4, Just 3, Nothing]
        -- Error: [4,3,*** Exception: minimum: empty structure
        -- print $ minimum ((Left 3) :: Either Int String)
        -- Error: *** Exception: minimum: empty structure
        print $ sum (7, 5)
        print $ fmap sum [(7, 5), (3, 4)]
        print $ fmap sum (Just [1, 2, 3, 4, 5])
        print $ product Nothing
        print $ fmap product (Just [])
        print $ fmap product (Right [1, 2, 3] :: Either String [Int])
