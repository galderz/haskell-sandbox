module HaskellBook.Chapter11.ExercisesJammin where

totalJarJams = 
  foldr (\x y -> y + count x) 0 allJam

row1 = JamJars Apple 5


row2 = JamJars Peach 2


row3 = JamJars Blackberry 10


row4 = JamJars Plum 3


row5 = JamJars Peach 8


row6 = JamJars Blackberry 3


allJam = [row1, row2, row3, row4, row5, row6]


-- Cardinality: 4 * number of possible ints
data JamJars = 
  JamJars {
    fruit :: Fruit,
    count :: Int
  } deriving (Eq, Show, Ord)


data Fruit =    Peach  | Plum  | Apple  | Blackberry
  deriving (Eq, Show, Ord)
  

--data JamJars =--  Jam Fruit Int
--  deriving (Eq, Show)
  
