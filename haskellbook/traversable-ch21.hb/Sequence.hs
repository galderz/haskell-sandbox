main :: IO ()
main =
    do  -- Compare these:
        print $ sum [1, 2, 3]
        print $ fmap sum [Just 1, Just 2, Just 3]
        print $ (fmap . fmap) sum Just [1, 2, 3]
        print $ fmap product [Just 1, Just 2, Nothing]
        -- To these:
        print $ fmap Just [1, 2, 3]
        print $ sequenceA $ fmap Just [1, 2, 3]
        print $ sequenceA [Just 1, Just 2, Just 3]
        print $ sequenceA [Just 1, Just 2, Nothing]
        print $ fmap sum $ sequenceA [Just 1, Just 2, Just 3]
        print $ fmap product (sequenceA [Just 3, Just 4, Nothing])
