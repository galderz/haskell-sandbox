module HaskellBook.Chapter10.FoldLeft where


-- Helper method that shows how foldl associates
showFoldl :: [String] -> String
showFoldl xs = 
    foldl (\x y -> concat ["(",x,"+",y,")"]) "0" xs

