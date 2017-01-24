module HaskellBook.Chapter09.Lists where


myTail' :: [a] -> [a]
myTail' (_ : xs) = xs
myTail' [] = []


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
