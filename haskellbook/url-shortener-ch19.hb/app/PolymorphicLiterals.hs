{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Text (Text)


main :: IO ()
main =
    do  print $ ("blah" :: Text) == fromString ("blah" :: String)
        print $ (1 :: Int) == fromInteger (1 :: Integer)
        print $ (2.5 :: Double) == fromRational (2.5 :: Rational)
