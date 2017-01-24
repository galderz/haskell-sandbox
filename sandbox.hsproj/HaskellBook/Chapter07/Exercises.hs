module HaskellBook.Chapter07.Exercises where

g :: (a -> b) -> (a, c) -> (b , c)
g f (x,y) = (f x, y)
--g f t = (f (fst t), snd t)

foldBool' :: a -> a -> Bool -> a
foldBool' x y b = z
  where z = if b then x else y

foldBool :: a -> a -> Bool -> a
foldBool x y True = x
foldBool x y False = y

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = fst (x `divMod` 10)
        d     = snd (xLast `divMod` 10)

hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = x `div` 100
        d     = xLast `mod` 100

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF a = (read . show) a

roundTripAB :: (Show a, Read b) => a -> b
roundTripAB a = read (show a)

roundTripABPF :: (Show a, Read b) => a -> b
roundTripABPF a = (read . show) a
