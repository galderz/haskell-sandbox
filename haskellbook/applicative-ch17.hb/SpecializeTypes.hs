import Data.Char

main :: IO ()
main =
    do  print ((pure 4) :: [Int])
        print ([(+1)] <*> [4])
        pure 'a' :: IO Char
        (pure ord) <*> (pure 'a' :: IO Char)
        print ""
