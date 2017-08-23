import Control.Monad
import Control.Monad.Trans.State


fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 15 == 0 =
      "FizzBuzz"
    | n `mod` 5 == 0 =
      "Buzz"
    | n `mod` 3 == 0 =
      "Fizz"
    | otherwise =
      show n



fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
    execState (mapM_ addResult list) []


addResult :: Integer -> State [String] ()
addResult n =
    do  xs <- get
        let result =
                fizzBuzz n
        put (result : xs)


fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to =
    fizzbuzzList $ enumBackwards from to


enumBackwards :: Integer -> Integer -> [Integer]
enumBackwards from to =
    foldr (\x z -> ((to + 1) - x) : z) [] [from..to]


main :: IO ()
main =
    mapM_ putStrLn $ fizzbuzzFromTo 1 100
