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
        print $ parseString (char 'a') mempty "abcdef"
        let stop = unexpected "stop pls"
        print $ (parseString (char 'a' >> stop) mempty "abcdef" :: Result String)
        print $ parseString (string "abc") mempty "abcdef"
        print $ (parseString (string "abc" >> stop) mempty "abcdef" :: Result String)
