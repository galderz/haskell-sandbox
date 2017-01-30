module HaskellBook.Chapter07.FuncPatterns where

myNum :: Integer
myNum = 1

-- Num a => a -> a
myVal = myNum

myVal' f = myNum

stillAFunction a b c = a ++ b ++ c

addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x = let y = 5 in 
  "the integer was: " ++ show x
  ++ " and y was: " ++ show y
  
-- Error: Not in scope: 'y'
--bindExp' :: Integer -> String
--bindExp' x = let z = y + x in 
--  let y = 5 in "the integer was: " 
--  ++ show x ++ " and y was: " 
--  ++ show y ++ " and z was: " + show z

shadowedBindExp :: Integer -> String
shadowedBindExp x = let x = 10; y = 5 in 
  "the integer was: " ++ show 0
  ++ " and y was: " ++ show y
  
triple :: Integer -> Integer
triple x = x * 3

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

-- Order matters!
isItTwo' :: Integer -> Bool
isItTwo' _ = False
isItTwo' 2 = True

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
  | RegisteredUser Username AccountNumber
  
printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
                          = putStrLn $ name ++ " " ++ show acctNum
            
data WherePenguinsLive =
    Galapagos
  | Antarctic
  | Australia
  | SouthAfrica
  | SouthAmerica deriving (Eq, Show)

data Penguin =  Peng WherePenguinsLive
  deriving (Eq, Show)
  
-- is it South Africa? If so, return TrueisSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctic = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

-- Better, less redundant
isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctic) = True
antarcticPenguin _ = False

-- in this final function, the || operator-- is an `or` function, which will return True
-- if either value is True
antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y = ((snd x, snd y), (fst x,fst y))

f' :: (a,b) -> (c,d)-> ((b,d),(a,c))
f' (a, b) (c, d) = ((b,d),(a,c))

-- These have to be the same type because 
--(+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- addEmUp2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a,b,c) -> c
third3 (_, _, x) = x

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs = 
  case xs == reverse xs of
    True -> "yes"
    False -> "no"
    
-- If you need to reuse computations, you can use where, e.g.
pal' xs = 
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyyyy. What's shakin'?"
    False -> putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"
  
returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

-- -> is right associative, so without parentheses, this is same as:
returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

data Employee = Coder
                | Manager
                | Veep
                | CEO
                deriving (Eq, Ord, Show)
                
reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'
  
employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = 
  case compare e e' of
      GT -> reportBoss e e'
      EQ -> putStrLn "Neither employee is the boss"
      LT -> (flip reportBoss) e e'
      -- LT -> reportBoss e' e  

employeeRank' :: (Employee -> Employee -> Ordering) 
                -> Employee 
                -> Employee 
                -> IO ()
employeeRank' f e e' = 
  case f e e' of
      GT -> reportBoss e e'
      EQ -> putStrLn "Neither employee is the boss"
      LT -> (flip reportBoss) e e'
      -- LT -> reportBoss e' e  

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"
  
isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c 
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"
  
dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y < 0/59  = 'F'
  where y = x / 100
  
add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne' :: Int -> Int
addOne' = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne' 0)
  print (addOnePF 0)
  print((addOne' . addOne')0)
  print ((addOnePF . addOne') 0)
  print((addOne' . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne' 0))
  print ((negate . addOne') 0)
  print ((addOne' . addOne' . addOne' . negate . addOne') 0)
  
print' :: Show a => a -> IO ()
print' a = putStrLn (show a)

print'' :: Show a => a -> IO ()
print'' a = (putStrLn . show) a

print''' :: Show a => a -> IO ()
print''' = (putStrLn . show)
