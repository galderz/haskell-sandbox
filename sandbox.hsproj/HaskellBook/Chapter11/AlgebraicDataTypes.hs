module HaskellBook.Chapter11.AlgebraicDataTypes where


-- newtype is similar to a type synonym
-- difference is: you can define typeclass instances
-- for newtypes that differ from instances for their
-- underlying type

class TooMany a where
    tooMany :: a -> Bool
    
instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where    tooMany (Goats n) = n > 43

-- newtype: cannot be a product type, sum type, or container nullary constructors.
-- advantage: no runtime overhead


newtype Goats =
    Goats Int deriving (Eq, Show)


newtype Cows =
    Cows Int deriving (Eq, Show)
    
-- Now we can rewrite our type to be-- safer, pattern matching in order-- to access the Int inside our data-- constructor Goats.
tooManyGoats2 :: Goats -> Bool
tooManyGoats2 (Goats n) = 
    n > 42


-- Problem if we had different limits for different type of livestock. What if we mixed Goats with Cows?
tooManyGoats :: Int -> Bool
tooManyGoats n =
    n > 42

-- Unary constructor
data Example2 = MakeExample2 Int deriving Show


-- Nullary constructor
data Example = MakeExample deriving Show


