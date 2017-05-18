
main :: IO ()
main =
    do  print $ const <$> Just "Hello" <*> pure "World"
