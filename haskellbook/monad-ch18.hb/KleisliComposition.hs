import Control.Monad ((>=>))


sayHi :: String -> IO String
sayHi greeting =
    do  putStrLn greeting
        getLine


readM :: Read a => String -> IO a
readM =
    return . read


getAge :: String -> IO Int
getAge =
    sayHi >=> readM


askForAge :: IO Int
askForAge =
    getAge "Hello! How old are you?"


main :: IO ()
main =
    do  age <- askForAge
        print age
