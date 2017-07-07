main :: IO ()
main =
    do  print $ fmap Just [1, 2, 3]
        print $ sequenceA $ fmap Just [1, 2, 3]
        print $ sequenceA . fmap Just $ [1, 2, 3]
        print $ traverse Just [1, 2, 3]
