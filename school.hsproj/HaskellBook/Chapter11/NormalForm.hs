module HaskellBook.Chapter11.NormalForm where
  

data Fiction = Fiction deriving Show


data Nonfiction = Nonfiction deriving Show


data BookType = 
    FictionBook Fiction    | NonfictionBook Nonfiction    deriving Show
    

type AuthorName = String


data Author = Author (AuthorName, BookType)


-- After applying distributive property of:
-- a * (b + c) -> (a * b) + (a * c)


-- In normal form:
data Author' =
    Fiction' AuthorName
    | Nonfiction' AuthorName deriving (Eq, Show)    
    

-- Already in normal form
data Expr =    Number Int  | Add Expr Expr  | Minus Expr  | Mult Expr Expr  | Divide Expr Expr


data FlowerType = 
    Gardenia    | Daisy    | Rose    | Lilac    deriving Show

type Gardener = String

data Garden =  Garden Gardener FlowerType  deriving Show
  
-- Normal form:

data Garden' =  Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Lilac' Gardener
  deriving Show




