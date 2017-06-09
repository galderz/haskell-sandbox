import Control.Applicative (liftA3)

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


data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" =
    Nothing
noEmpty str =
    Just str


noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing


-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing ->
            Nothing
        Just nammy ->
            case noNegative age' of
                Nothing ->
                    Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing ->
                            Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' =
    do  nammy <- noEmpty name'
        agey <- noNegative age'
        weighty <- noNegative weight'
        weightCheck (Cow nammy agey weighty)


f :: Integer -> Maybe Integer
f 0 =
    Nothing
f n =
    Just n


g :: Integer -> Maybe Integer
g i =
    if even i
    then Just (i + 1)
    else Nothing


h :: Integer -> Maybe String
h i =
    Just ("10191" ++ show i)

-- Can't be re-written with Applicative because you need previous value
doSomething :: Integer -> Maybe (Integer, Integer, String)
doSomething n =
    do  a <- f n
        b <- g a
        c <- h b
        pure (a, b, c)


doSomething' :: Integer -> Maybe (Integer, Integer, String)
doSomething' n =
    case f n of
        Nothing ->
            Nothing
        Just x ->
            case g x of
                Nothing ->
                    Nothing
                Just y ->
                    case h y of
                        Nothing ->
                            Nothing
                        Just z ->
                            Just (x, y, z)


main :: IO ()
main =
    do  print $ twiceWhenEven [1..3]
        print $ twiceWhenEven' [1..3]
        print $ mkSphericalCow "Bess" 5 499
        print $ mkSphericalCow "Bess" 5 500
        print $ mkSphericalCow' "Bess" 5 499
        print $ mkSphericalCow' "Bess" 5 500
        print $ doSomething 2
        print $ doSomething 0
        print $ doSomething 1
        print $ doSomething' 2
        print $ doSomething' 0
        print $ doSomething' 1
