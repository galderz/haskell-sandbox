import Text.Trifecta


main :: IO ()
main =
    do
        let gimmeA = char 'a'
        print $ parseString gimmeA mempty "a"
        print $ parseString gimmeA mempty "b"
        print $ parseString (char 'b') mempty "b"
        print $ parseString (char 'b' >> char 'c') mempty "b"
        print $ parseString (char 'b' >> char 'c') mempty "bc"
        print $ parseString (char 'b' >> char 'c') mempty "abc"
        print $ parseString (string "abc") mempty "abc"
        print $ parseString (string "abc") mempty "bc"
        print $ parseString (string "abc") mempty "ab"
