module HaskellBook.Chapter03.Exercises where
  
appendCurry x = "Curry is awesome" ++ x

indexCurry n = "Curry is awesome" !! n

dropCurry n = drop n "Curry is awesome"

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String -> String
rvrs x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
