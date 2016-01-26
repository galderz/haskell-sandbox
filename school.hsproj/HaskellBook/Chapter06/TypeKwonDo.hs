module HaskellBook.Chapter06.TypeKwonDo where
  
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = b == f a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = (f a) + (fromInteger n)
