import Text.Trifecta


main :: IO ()
main =
    do  print $ parseString integer mempty "123abc"
        print $ parseString (integer >> eof) mempty "123abc"
        print $ parseString (integer >> eof) mempty "123"
        print $ parseString (integer >>= (\x -> eof >> pure x)) mempty "123"
