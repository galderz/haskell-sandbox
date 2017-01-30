module HaskellBook.Chapter11.ProductTypes where


-- Using record syntax.
-- Defining it as a record means 
-- there are now named field accessors
data PersonR = 
    Person { 
        name :: String
        , age::Int }
        deriving (Eq, Show)


jm' = Person "julie" 108
ca' = Person "chris" 16
  

data Person = MkPerson String Int deriving (Eq, Show)


-- these are just sample data
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16


namae :: Person -> String
namae (MkPerson s _) = s


-- TwoQs could have been written with a type alias
-- Same cardinality: 6
type TwoQsAlias = (QuantumBool, QuantumBool)


-- Cardinality: 3 * 2 = 6
data TwoQs =    MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)


-- Cardinality: 3
data QuantumBool = 
    QuantumTrue    | QuantumFalse    | QuantumBoth
    deriving (Eq, Show)
    
