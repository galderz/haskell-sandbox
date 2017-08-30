import Text.Trifecta


main :: IO ()
main =
    do  print $ parseString integer mempty "123abc"
