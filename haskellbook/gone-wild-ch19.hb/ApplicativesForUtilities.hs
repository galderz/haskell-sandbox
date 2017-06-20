import Control.Applicative


(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) =
    liftA2 (||)



f :: (Eq a, Num a) => a -> Bool
f 9001 =
    True
f _ =
    False


g :: (Eq a, Num a) => a -> Bool
g 42 =
    True
g _ =
    False


main :: IO ()
main =
    do  print $ True || False
        print $ False || False
        print $ (2 > 3) || (3 == 3)
        print $ f 42
        print $ f 9001
        print $ g 42
        print $ g 9001
        -- We can compose the two functions f and g to take one input
        -- and give summary result like this
        print $ (\n -> f n || g n) 0
        print $ (\n -> f n || g n) 9001
        -- But we have to pass in that argument n to do it that way.
        -- The utility function gives us a cleaner way
        print $ (f <||> g) 0
        print $ (f <||> g) 9001
