module HaskellBook.Chapter11.FunctionAsExponential where


-- a -> b 
-- b ^ a

-- a -> b -> c
-- (c ^ b) ^ a
-- c ^ (b * a)


data Quantum =
  Yes
  | No
  | Both
  deriving (Eq, Show)
    

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes


quantSum2 :: Either Quantum Quantum
quantSum2 = Right No


quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both


quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

-- ... 

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

-- ...


-- 3 ^ 3

quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes


quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No


quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both


quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes


quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes


quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes


quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes


quantFlip8 :: Quantum -> Quantum  
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No


quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No


quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both

-- ...

-- 2 ^ 3 = 8
convertFlip1 :: Quantum -> Bool
convertFlip1 Yes  = True
convertFlip1 No   = True
convertFlip1 Both = True


convertFlip2 :: Quantum -> Bool
convertFlip2 Yes  = True
convertFlip2 No   = True
convertFlip2 Both = False


convertFlip3 :: Quantum -> Bool
convertFlip3 Yes  = True
convertFlip3 No   = False
convertFlip3 Both = True


convertFlip4 :: Quantum -> Bool
convertFlip4 Yes  = True
convertFlip4 No   = False
convertFlip4 Both = False


convertFlip5 :: Quantum -> Bool
convertFlip5 Yes  = False
convertFlip5 No   = True
convertFlip5 Both = True


convertFlip6 :: Quantum -> Bool
convertFlip6 Yes  = False
convertFlip6 No   = True
convertFlip6 Both = False


convertFlip7 :: Quantum -> Bool
convertFlip7 Yes  = False
convertFlip7 No   = False
convertFlip7 Both = True


convertFlip8 :: Quantum -> Bool
convertFlip8 Yes  = False
convertFlip8 No   = False
convertFlip8 Both = False


data Quad 
  = One  | Two  | Three  | Four  deriving (Eq, Show)


-- how many different forms can this take?eQuad :: Either Quad Quad
eQuad = undefined
-- ^ Sum, so: 4 + 4 = 8

-- how many different forms can this take?
prodQuad :: (Quad, Quad)
prodQuad = undefined
-- ^ Sum, so: 4 * 4 = 16

-- how many different forms can this take?
funcQuad :: Quad -> Quad
funcQuad = undefined
-- ^ Fun, so: 4 ^ 4 = 256

-- how many different forms can this take?
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- Sum, so: 2 + 2 + 2 = 6

-- how many different forms can this take?
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- Fun, so: 2 ^ 2 ^ 2 = 16

-- how many different forms can this take?
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- Fun, so: (4 ^ 4) ^ 2 = 65,536



