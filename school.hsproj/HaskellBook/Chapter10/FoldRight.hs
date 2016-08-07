module HaskellBook.Chapter10.FoldRight where


-- Helper method that shows how foldr associates
showFoldr :: [String] -> String
showFoldr xs = 
    foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs

