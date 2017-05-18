
main :: IO ()
main =
    do  print $ pure id <*> [1..5]
        print $ pure id <*> Just "Hello Applicative"
        -- print $ pure id <*> Nothing
        -- print $ pure id <*> Left "Erro'ish"
        -- print $ pure id <*> Right 8001
        print $ pure id <*> (+1) $ 2
