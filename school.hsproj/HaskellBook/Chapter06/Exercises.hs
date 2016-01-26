module HaskellBook.Chapter06.Exercises where

import Data.List (sort)

data TisAnInteger =
  TisAn Integer
  
instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn n') = n == n'

data TwoIntegers =
  Two Integer Integer
  
instance Eq TwoIntegers where
  (==) (Two n1 n2) (Two n1' n2') = 
    n1 == n1' && n2 == n2'

data StringOrInt =
    TisAnInt Int
  | TisAString String
  
instance Eq StringOrInt where
  (==) (TisAnInt n) (TisAnInt n') = n == n'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False
  
data Pair a =
  Pair a a
  
instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = 
    a == a' && b == b'

data Tuple a b =
  Tuple a b
  
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =
    a == a' && b == b'
    
data Which a =
    ThisOne a 
  | ThatOne a
  
instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

data Person = Person Bool
  deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot 
  deriving (Show, Eq)

settleDown x = if x == Woot
                  then Blah
                  else x

type Subject = String
type Verb = String
type Object = String

data Sentence = 
  Sentence Subject Verb Object 
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = 
  Rocks String deriving (Eq, Show)
data Yeah = 
  Yeah Bool deriving (Eq, Show)
data Papu = 
  Papu Rocks Yeah deriving (Eq, Show)
  
--phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'

i :: Num a => a
--i :: a -- does not compile
i = 1

--f :: Float
--f :: Num a => a
--f :: Fractional a => a
f :: RealFrac a => a
f = 1.0

--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

freud' :: a -> a
--freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
--sigmund :: a -> a
sigmund x = myX

sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a
sigmund' x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
