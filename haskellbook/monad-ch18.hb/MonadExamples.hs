import Control.Monad (join)


j :: Monad m => m (m a) -> m a
j =
    join


main :: IO ()
main =
    do  print $ j [[1, 2], [], [3]]
        print $ j (Just (Just 1))
        print $ j ((Just Nothing) :: Maybe (Maybe Int))
        print $ j (Nothing :: Maybe (Maybe Int))
