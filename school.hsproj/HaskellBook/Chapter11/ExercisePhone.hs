module HaskellBook.Chapter11.ExercisePhone where


import Data.Char
import Data.List


{-|
 ---------------------------
 | 1      | 2 ABC | 3 DEF  |
 ___________________________
 | 4 GHI  | 5 JKL | 6 MNO  |
 ---------------------------
 | 7 PQRS | 8 TUV | 9 WXYZ |
 ---------------------------
 | * ^    | 0 + _ | # .,   |
 ---------------------------
-}

type Digit = Char


-- Valid presses: 1 and up
type Presses = Int


data DaPhone = DaPhone [(Char, Digit, Presses)]


buttons = DaPhone 
  [ ('a', '2', 1),  ('b', '2', 2), ('c', '2', 3)
  , ('d', '3', 1),  ('e', '3', 2), ('f', '3', 3)
  , ('g', '4', 1),  ('h', '4', 2), ('i', '4', 3)
  , ('j', '5', 1),  ('k', '5', 2), ('l', '5', 3)
  , ('m', '6', 1),  ('n', '6', 2), ('o', '6', 3)
  , ('p', '7', 1),  ('q', '7', 2), ('r', '7', 3), ('s', '7', 4)
  , ('t', '8', 1),  ('u', '8', 2), ('v', '8', 3)
  , ('w', '9', 1),  ('x', '9', 2), ('y', '9', 3), ('z', '9', 4)
  , (' ', '0', 1)
  , ('.', '#', 1), (',', '#', 2) 
  ]
  
--  [ ('1', ""),     ('2', "abc"), ('3', "def")
--  , ('4', "ghi"),  ('5', "jkl"), ('6', "mno")
--  , ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz") 
--  , ('*', "^"),    ('0', "+ "),  ('#', ".,")
--  ]
  

convo :: [String]
convo =
  ["Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]
  

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps dp c
  | isUpper c = 
      ('*', 1) : reverseTapsIgnoreCase dp (toLower c)
  | otherwise = 
      reverseTapsIgnoreCase dp c


reverseTapsIgnoreCase :: DaPhone -> Char -> [(Digit, Presses)]
reverseTapsIgnoreCase dp c = 
  map (\p -> extractSndThd p) (findPhoneChar dp c)


findPhoneChar :: DaPhone -> Char -> [(Char, Digit, Presses)]
findPhoneChar (DaPhone ph) c =
  filter (\(ch, _, _) -> c == ch) ph


extractSndThd :: (Char, Digit, Presses) -> (Digit, Presses)
extractSndThd (c, d, p) = 
  (d, p)


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead dp s = 
  concat (map (\c -> reverseTaps dp c) s)


fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps ps =
  foldr (\(_, p) acc -> acc + p) 0 ps


mostPopularLetter :: String -> Char
mostPopularLetter = 
  snd . mostPopularLetterFrequency


mostPopularLetterFrequency :: String -> (Int, Char)
mostPopularLetterFrequency str =
  maximum $ filter (\t -> isLetter (snd t)) (frequency str)


frequency :: Ord a => [a] -> [(Int, a)]
frequency list = 
  map (\l -> (length l, head l)) (group (sort list))
  

costOfPopularLetter :: String -> Presses
costOfPopularLetter str =
  fst freq * (fingerTaps (reverseTaps buttons (snd freq)))
  where freq =
          mostPopularLetterFrequency str


coolestLtr :: [String] -> Char
coolestLtr l =
  mostPopularLetter (concat l)


frequencyWord :: String -> [(Int, String)]
frequencyWord txt = 
  map (\l -> (length l, head l)) (group (sort (words txt)))
  

mostPopularWordFrequency :: String -> (Int, String)
mostPopularWordFrequency str =
  maximum (frequencyWord str)


coolestWord :: [String] -> String
coolestWord txt = 
  snd (mostPopularWordFrequency (unwords txt))

