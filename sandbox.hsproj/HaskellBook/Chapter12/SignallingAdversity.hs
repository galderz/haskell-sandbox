module HaskellBook.Chapter12.SignallingAdversity where
  

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = 
  if even n then Just (n+2) else Nothing


type Name = String
type Age = Integer

  
data Person = Person Name Age deriving Show


mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing
  

data PersonInvalid = 
  NameEmpty
  | AgeTooLow
  deriving (Eq, Show)
  

-- Compiles fine without PersonInvalid deriving Eq
toString :: PersonInvalid -> String
toString NameEmpty = 
  "NameEmpty"
toString AgeTooLow =
  "AgeTooLow"
  

-- If not providing deriving Show for 
--instance Show PersonInvalid where
--  show = toString
  

-- This does not work without an Eq instance
blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = 
    "NameEmpty"
  | pi == AgeTooLow = 
    "AgeTooLow"
  | otherwise = 
    "???"
    

-- Problem is that if multiple errors found, only one returned
mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 name age 
  | name /= "" && age >= 0 = 
      Right $ Person name age
  | name == "" = 
      Left NameEmpty
  | otherwise = 
      Left AgeTooLow


type ValidatePerson a = Either [PersonInvalid] a


-- Checking functions
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = 
  case age >= 0 of
    True -> 
      Right age
    False -> 
      Left [AgeTooLow]


nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = 
  case name /= "" of
    True -> 
      Right name
    False -> 
      Left [NameEmpty]


mkPerson3 :: Name -> Age -> ValidatePerson Person
mkPerson3 name age =
  mkPerson3' (nameOkay name) (ageOkay age)
  

mkPerson3' :: ValidatePerson Name 
  -> ValidatePerson Age
  -> ValidatePerson Person
mkPerson3' (Right nameOk) (Right ageOk) = 
  Right (Person nameOk ageOk)
mkPerson3' (Left badName) (Left badAge) =
  Left (badName ++ badAge)
mkPerson3' (Left badName) _ = 
  Left badName  
mkPerson3' _ (Left badAge) = 
  Left badAge  
  


