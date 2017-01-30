module HaskellBook.Chapter11.Exercises where


import Data.Char
import Data.List (groupBy)
import Data.Function (on)


data Weekday =
  Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday


doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs


isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xx@(x:xs) (y:ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xx ys


isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] ys =
  True
isSubsequenceOf' (x:xs) ys =
  elem x ys && isSubsequenceOf' xs ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map f (words xs)
  where f w@(y:ys) = (w, toUpper y : ys)


capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (' ':xs) = " " ++ capitalizeWord xs
capitalizeWord (x:xs) = (toUpper x) : xs


capitalizeParagraph :: String -> String
capitalizeParagraph ws = findWord ws ""


findWord :: String -> String -> String
findWord [] acc = capitalizeWord (reverse acc)
findWord (w:ws) acc
  | w == '.'  = capitalizeWord (reverse (w : acc)) ++ (findWord ws "")
  | otherwise = findWord ws (w : acc)


capitalizeWord' :: String -> String
capitalizeWord' [] = []
capitalizeWord' (x:xs)
    | isAlpha x = toUpper x : xs
    | otherwise = x : capitalizeWord' xs


capitalizeParagraph' :: String -> String
capitalizeParagraph' xs = 
  concatMap capitalizeWord' $ groupBy ((==) `on` (=='.')) xs

--findWord :: String -> String -> (String, String)
--findWord (w:ws) acc
--  | w == ' '  = (reverse (w : acc), ws)
--  | otherwise = findWord ws (w : acc)


--capitalizeParagraph :: String -> String
--capitalizeParagraph ws = 
--  concat (map (\w -> capitalizeWord w) (words ws))
