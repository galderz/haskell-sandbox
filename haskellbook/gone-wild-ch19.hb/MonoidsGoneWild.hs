import Data.Monoid


f :: b -> Sum Integer
f =
    const (Sum 1)


g :: b -> Sum Integer
g =
    const (Sum 2)


main :: IO ()
main =
    do  print $ f 9001
        print $ g 9002
        -- (a -> b) <> (a -> b)
        -- mappends the result of two function applications,
        -- as long as b has a Monoid instance
        print $ (f <> g) 9001
