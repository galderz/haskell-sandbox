module HangmanTests where


import Test.Hspec


data Puzzle =
    Puzzle String [Maybe Char] [Char]
    deriving (Eq, Show)


freshPuzzle :: String -> Puzzle
freshPuzzle x =
  Puzzle x (map (const Nothing) x) []


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


main :: IO ()
main = hspec $ do
    describe "Fresh puzzle" $ do
        it "can be used to create a Puzzle" $ do
            freshPuzzle "haskell" `shouldBe`
                (Puzzle "haskell" (replicate 7 Nothing) "")
    describe "Fill in character" $ do
        it "might not find a character" $ do
            fillInCharacter (freshPuzzle "haskell") 'z' `shouldBe`
                (Puzzle "haskell" (replicate 7 Nothing) "z")
        it "might find a character" $ do
            fillInCharacter (freshPuzzle "haskell") 'l' `shouldBe`
                (Puzzle "haskell" ((replicate 5 Nothing) ++ [Just 'l', Just 'l']) "")
