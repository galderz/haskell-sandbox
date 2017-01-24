module HaskellBook.Chapter11.DataConstructors where


data Example0 =    Example0 deriving (Eq, Show)
    

-- unary
data Example1 =
    Example1 Int deriving (Eq, Show)


-- product of Int and String
data Example2 =
    Example2 Int String deriving (Eq, Show)


data Doggies a = 
    Husky a    | Mastiff a 
    deriving (Eq, Show)
    
-- type constructor awaiting an argumentDoggies


data PugType = PugData


myPug = PugData :: PugType


data HuskyType a = HuskyData


myHusky :: HuskyType a
myHusky = HuskyData


myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData


myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

-- no witness to the contrary


data DogueDeBordeaux doge = DogueDeBordeaux doge


-- This will work because the value 10 agrees 
-- with the type variable being bound to Int myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10


-- This will not work because 10-- cannot be reconciled with the
-- type variable being bound to String
--badDoge :: DogueDeBordeaux String
--badDoge = DogueDeBordeaux 10

