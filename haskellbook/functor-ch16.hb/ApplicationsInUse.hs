import Control.Applicative


main :: IO()
main =
    do  print $ fmap (2^) [1,2,3]
        print $ fmap (^2) [1,2,3]
        print $ [(+1), (*2)] <*> [2, 4]
        -- print $ fmap (,) [1]
        print $ fmap (+1) (1, 2)
        print $ (,) <$> [1, 2] <*> [3, 4] --  [(1, ), (2, )] <*> [3, 4]
        print $ liftA2 (,) [1,2] [3,4]
        print $ (+) <$> [1, 2] <*> [3, 5]
        print $ liftA2 (+) [1,2] [3,5]
        print $ max <$> [1,2] <*> [1,4]
        print $ liftA2 max [1,2] [1,4]
