
main :: IO ()
main =
    do  -- Identity laws
        print $ pure id <*> [1..5]
        print $ pure id <*> Just "Hello Applicative"
        -- print $ pure id <*> Nothing
        -- print $ pure id <*> Left "Erro'ish"
        -- print $ pure id <*> Right 8001
        print $ pure id <*> (+1) $ 2
        print $ id [1..5] == fmap id [1..5]
        print $ fmap id [1..5] == (pure id <*> [1..5])
        print $ (pure id <*> [1..5]) == id [1..5]
        -- Composition laws
        print $ (pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]) ==
            ([(+1)] <*> ([(*2)] <*> [1, 2, 3]))
        print $ (pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1) ==
            (Just (+1) <*> (Just (*2) <*> Just 1))
