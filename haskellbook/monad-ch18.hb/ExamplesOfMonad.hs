
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs =
    do  x <- xs
        if even x
            then [x * x, x* x]
            else [x * x]


twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
    do  x <- xs
        if even x
            then [x * x, x* x]
            else []


main :: IO ()
main =
    do  print $ twiceWhenEven [1..3]
        print $ twiceWhenEven' [1..3]
