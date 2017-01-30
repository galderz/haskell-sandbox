module HaskellBook.Chapter11.DataVsType where


data Size = 
    Size Integer
    deriving (Eq, Show)


getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined


isCar :: Vehicle -> Bool
isCar (Car _ _) = 
    True
isCar _ = 
    False


isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = 
    True
isPlane _ = 
    False


areCars :: [Vehicle] -> [Bool]
areCars =
    map isCar


myCar = 
    Car Mini (Price 14000)


urCar = 
    Car Mazda (Price 20000)


clownCar = 
    Car Tata (Price 7000)


doge = 
    Plane PapuAir (Size 1000)


data Vehicle =                -- (k)
    Car Manufacturer Price    -- (l) [2] [3]
    | Plane Airline Size      -- (m) [4] [5]
    deriving (Eq, Show)
-- type constructor (k)-- two data constructors (l) and (m).-- four type arguments [2], [3], [4] and [5]-- two type arguments to (l) are [2] and [3]-- type arguments to (m) are [4] and [5]


data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)
-- one type constructor
-- three data constructors


data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)
-- one type constructor
-- three data constructors


data Price =
    Price Integer -- type in data area!
    deriving (Eq, Show)


