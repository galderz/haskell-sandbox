import Text.Trifecta


main :: IO ()
main =
    do
        let gimmeA = char 'a'
        print $ parseString gimmeA mempty "a"
        print $ parseString gimmeA mempty "b"
        print $ parseString (char 'b') mempty "b"
        print $ parseString (char 'b' >> char 'c') mempty "b"
