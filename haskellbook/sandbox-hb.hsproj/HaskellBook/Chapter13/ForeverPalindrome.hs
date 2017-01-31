module HaskellBook.Chapter13.ForeverPalindrome where

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)


isPalindrome :: String -> Bool
isPalindrome sentence = chars == reverse chars
  where chars = map toLower $ filter isLetter sentence

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True ->
      putStrLn "It's a palindrome!"
    False ->
      do putStrLn "Nope!"
         exitSuccess
