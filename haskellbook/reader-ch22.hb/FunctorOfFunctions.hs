main :: IO ()
main =
    do  print $ fmap (+1) (*2) 3
        print $ fmap (+1) (*2) $ 3
        print $ (fmap (+1) (*2)) 3
        print $ (+1) . (*2) $ 3
        print $ (+1) `fmap` (*2) $ 3
