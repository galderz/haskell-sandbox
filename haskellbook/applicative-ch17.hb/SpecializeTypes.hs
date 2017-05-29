
main :: IO ()
main =
    do  print ((pure 4) :: [Int])
        print ([(+1)] <*> [4])
