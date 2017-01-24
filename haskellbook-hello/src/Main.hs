module Main where

import DogsRule
import Hello
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  -- ^ so that putStr isn't buffer (deferred) and prints immediately
  putStr "Please inpurt your name: "
  name <- getLine
  sayHello name
  dogs
