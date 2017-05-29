import Data.Char

main :: IO ()
main =
    do  print ((pure 4) :: [Int]) -- []
        print ([(+1)] <*> [4])
        pure 'a' :: IO Char -- IO
        (pure ord) <*> (pure 'a' :: IO Char)
        print ((pure 4) :: (String, Int)) -- (,) a
        print ((("boo", (+1)) <*> ("ahh", 4)))
