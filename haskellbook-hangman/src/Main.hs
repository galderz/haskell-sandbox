module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


-- type WordList = [String]

-- use new type for more clarity:
newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)


minWordLength :: Int
minWordLength = 5


maxWordLength :: Int
maxWordLength = 9


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in  l > minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl - 1))
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


data Puzzle = Puzzle String [Maybe Char] [Char]


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing =
  '_'
renderPuzzleChar (Just x) =
  x


freshPuzzle :: String -> Puzzle
freshPuzzle x =
  Puzzle x (map (const Nothing) x) []
-- Puzzle x (map (\c -> Nothing) x) []


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c =
  c `elem` s


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ l _) c =
  Just c `elem` l


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar newGuessed
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar
        newGuessed =
          if elem c word
          then s
          else c : s


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
        putStrLn "You already guesse that\
                  \ character, pick somethign else!"
        return puzzle
    (True, _) -> do
        putStrLn "This character was in the word,\
                  \ filling in the word accordignly"
        return (fillInCharacter puzzle guess)
    (False, _) -> do
        putStrLn "This character wasn't in \
                  \ the word, try again."
        return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else
    return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else
    return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is:" ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must\
                   \ be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
