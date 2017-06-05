
andOne x =
    [x, 1]


main :: IO ()
main =
    do  -- You can derive Functor in terms of Monad
        -- fmap f xs = xs >>= return . f
        print $ fmap (+1) [1..3]
        print $ [1..3] >>= return . (+1)
        -- Monad, in a sense, is a generalization of concat
        print $ andOne 10
        print $ fmap andOne [4, 5, 6]
        print $ concat $ fmap andOne [4, 5, 6]
