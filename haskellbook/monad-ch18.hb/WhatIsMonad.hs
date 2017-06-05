
main :: IO ()
main =
    do  -- You can derive Functor in terms of Monad
        -- fmap f xs = xs >>= return . f
        print $ fmap (+1) [1..3]
        print $ [1..3] >>= return . (+1)
