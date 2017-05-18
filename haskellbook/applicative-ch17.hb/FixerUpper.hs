
main :: IO ()
main =
    do  print $ const <$> Just "Hello" <*> pure "World"
        print $ (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
