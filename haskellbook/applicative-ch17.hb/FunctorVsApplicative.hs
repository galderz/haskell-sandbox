module FunctorVsApplicative where

-- You can define a Functor in terms of a provided Applicative instance:
-- fmap f x = pure f <*> x

main :: IO ()
main =
    do  print $ fmap (+1) [1,2,3]
        print $ pure (+1) <*> [1..3]
        print $ (pure 1 :: [Int])
        print $ (pure 1 :: Maybe Int)
        print $ (pure 1 :: Either String Int)
        print $ (pure 1 :: ([String], Int))
        --print $ (pure ((+1) :: (Int -> Int))
