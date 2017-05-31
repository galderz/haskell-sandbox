import Data.Char

f :: Int -> Int
f =
    pure 4

f' :: Int -> Int
f' =
    pure (+1) <*> pure 4


main :: IO ()
main =
    do  print ((pure 4) :: [Int]) -- []
        print ([(+1)] <*> [4])
        pure 'a' :: IO Char -- IO
        (pure ord) <*> (pure 'a' :: IO Char)
        print ((pure 4) :: (String, Int)) -- (,) a
        print ((("boo", (+1)) <*> ("ahh", 4)))
        -- print ((pure 4) :: (Int -> Int))
        -- print ((pure (+1) <*> 4) :: (Int -> Int))
        print $ f 5
        print $ f' 8
