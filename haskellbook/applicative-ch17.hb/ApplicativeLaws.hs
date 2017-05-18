
main :: IO ()
main =
    do  print $ pure id <*> [1..5]
        print $ pure id <*> Just "Hello Applicative"
        -- print $ pure id <*> Nothing
        -- print $ pure id <*> Left "Erro'ish"
        -- print $ pure id <*> Right 8001
        print $ pure id <*> (+1) $ 2
        print $ id [1..5] == fmap id [1..5]
        print $ fmap id [1..5] == (pure id <*> [1..5])
        print $ (pure id <*> [1..5]) == id [1..5]
