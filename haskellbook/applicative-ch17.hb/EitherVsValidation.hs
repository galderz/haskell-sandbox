

main :: IO ()
main =
    do  print (pure 1 :: Either String Int)
        print ((Right (+1) <*> Right 1) :: Either String Int)
        print $ Right (+1) <*> Left ":("
        print ((Left ":("  <*> Right 1) :: Either String Int)
        print ((Left ":(" <*> Left "sadface.png") :: Either String Int)
