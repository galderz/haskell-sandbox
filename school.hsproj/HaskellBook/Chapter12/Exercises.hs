module HaskellBook.Chapter12.Exercises where


-- String processing  


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x


wordOrA :: Maybe String -> String
wordOrA (Just w) = w
wordOrA Nothing = "a"


-- Supposed to be recursive?
replaceThe :: String -> String
replaceThe txt =
  unwords $ map (wordOrA . notThe) (words txt)

  
--countIfVowel :: Maybe String -> Int -> Int
--countIfVowel (Just w) i = i + 1
--countIfVowel Nothing i = i

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"


isStartVowel :: String -> Bool
isStartVowel (x:xs) = x `elem` "aeiou"
isStartVowel [] = False


countRec :: [String] -> Integer -> Integer
countRec (x:y:xs) count
  | x == "the" && isStartVowel y = 
      countRec xs (count + 1)
  | otherwise =
      countRec xs (count)
countRec _ count =
  count 


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s =
  countRec (words s) 0


countVowels :: String -> Integer
countVowels s = 
  foldr (\x z -> if (isVowel x) then z + 1 else z) 0 s


-- Validate the word


newtype Word' =
  Word' String
  deriving (Eq, Show)
  

vowels = "aeiou"


mkWord :: String -> Maybe Word'
mkWord w = 
  if (vCount > cCount) then Just (Word' w) else Nothing
  where vCount = countVowels w
        cCount = (toInteger (length w)) - vCount
    

-- It's Only Natural


data Nat = 
  Zero
  | Succ Nat
  deriving (Eq, Show)
  

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) =
  if (x == Zero) then 1 else 1 + natToInteger x


integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 =
      Nothing
  | x == 0 =
      Just Zero
  | otherwise =
      fmap (\x -> Succ x) (integerToNat (x - 1))


-- Small library for Maybe


isJust :: Maybe a -> Bool
isJust (Just _) = 
  True
isJust Nothing = 
  False


isNothing :: Maybe a -> Bool
isNothing Nothing = 
  True
isNothing _ = 
  False


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = 
  b
mayybee b f (Just a) = 
  f a


-- Try writing it in terms of the maybe catamorphism
fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma


fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = a
fromMaybe' _ (Just a) = a


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just a:xs) = a : catMaybes xs


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (Just a:xs) = fmap (\as -> a : as) (flipMaybe xs)


-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = 
  foldr (\a z -> f a z) []
  where 
      f (Left a) xs = 
          a : xs
      f (Right b) xs = 
          xs

--lefts' :: [Either a b] -> [a]
--lefts' xs = 
--  fst (foldr (\a z -> appendLeft a z) ([], False) xs)
--  where 
--      appendLeft (Left a) z = 
--          if (snd z) then ([], True) else ((a : fst z), snd z)
--      appendLeft (Right b) z = 
--          ([], True)

rights' :: [Either a b] -> [b]
rights' =
  foldr (\a z -> f a z) []
  where 
      f (Left a) xs = 
          xs
      f (Right b) xs = 
          b : xs


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = 
  foldr (\a z -> f a z) ([], [])
  where 
      f (Left a) xs = 
          (a : fst xs, snd xs)
      f (Right b) xs = 
          (fst xs, b : snd xs)


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f (Left _) = Nothing


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa fb (Right b) = fb b
either' fa fb (Left a) = fa a


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fbc e = 
  either' (\_ -> Nothing) (\b -> Just (fbc b)) e
