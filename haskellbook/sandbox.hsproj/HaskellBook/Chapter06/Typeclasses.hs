module HaskellBook.Chapter06.Typeclasses where

divideThenAdd x y = (x / y) + 1

-- Does not compile since / requires fractional
--divideThenAdd' :: Num a => a -> a -> a
--divideThenAdd' x y = (x / y) + 1

divideThenAdd' :: Fractional a => a -> a -> a
divideThenAdd' x y = (x / y) + 1

substractThenAdd :: Num a => a -> a -> a
substractThenAdd x y = (x - y) + 1

data Mood = Blah

instance Show Mood where
  show _ = "Blah"
  
data Mood' = Blah' deriving Show

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  -- defaultNumber :: a <- Bad practice
  
newtype Age = 
  Age Integer
  deriving (Eq, Show)
  
instance Numberish Age where 
  fromNumber n = Age n
  toNumber (Age n) = n
  -- defaultNumber = Age 65 <- Bad practice

newtype Year = 
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n 
  toNumber (Year n) = negate n
  -- defaultNumber = Year 1988 <- Bad practice
  
sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

data Trivial = 
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Ord, Show)
  
data Date = 
  Date DayOfWeek Int
  
instance Eq DayOfWeek where
  (==) Mon Mon    = True
  (==) Tue Tue    = True
  (==) Weds Weds  = True
  (==) Thu Thu    = True
  (==) Fri Fri    = True
  (==) Sat Sat    = True
  (==) Sun Sun    = True
  (==) _ _        = False
  
instance Eq Date where
  (==) (Date weekday monthNum)
       (Date weekday' monthNum') =
    weekday == weekday' && monthNum == monthNum'

data Identity a =
  Identity a
  
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data NoEqInstance = 
  NoEqInstance
  
add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1 then x + y else x
  
-- Compiles fine because anything with Ord has Eq too, but really Eq is what should be used here
check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

-- With concrete types:
add' :: Int -> Int -> Int
add' x y = x + y

addWeird' :: Int -> Int -> Int
addWeird' x y =
  if x > 1 then x + y else x
  
-- Compiles fine because anything with Ord has Eq too, but really Eq is what should be used here
check'' :: Int -> Int -> Bool
check'' a a' = a == a'
