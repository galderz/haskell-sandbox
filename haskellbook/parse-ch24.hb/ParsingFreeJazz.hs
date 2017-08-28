import Text.Trifecta


main :: IO ()
main =
    do
        let gimmeA = char 'a'
        print $ parseString gimmeA mempty "a"
