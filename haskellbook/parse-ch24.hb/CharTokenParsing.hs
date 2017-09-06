import Text.Trifecta

p' :: Parser [Integer]
p' =
    some $
    do  i <- token (some digit)
        return (read i)


main :: IO ()
main =
    do
        print $ parseString (some digit) mempty "123 456"
        print $ parseString (some (some digit)) mempty "123 456"
        print $ parseString (some integer) mempty "123"
        print $ parseString (some integer) mempty "123456"
        print $ parseString (some integer) mempty "123 456"
        print $ parseString (some integer) mempty "123\n\n 456"
        print $ parseString (some integer) mempty "123 \n \n 456"
        let s =
                "123 \n \n 456"
        print $ parseString (token (some digit)) mempty s
        print $ parseString (token (some (token digit))) mempty s
        print $ parseString (some decimal) mempty s
        print $ parseString (some (token decimal)) mempty s
        let s' =
                "1\n2\n3"
        print $ parseString p' mempty s'
        print $ parseString (token (some digit)) mempty s'
        print $ parseString (some (token (some digit))) mempty s'
        let tknWhole =
                token $ char 'a' >> char 'b'
        print $ parseString tknWhole mempty "a b"
        print $ parseString tknWhole mempty "ab ab"
        print $ parseString (some tknWhole) mempty "ab ab"
        let tknCharA =
                (token (char 'a')) >> char 'b'
        print $ parseString tknCharA mempty "a b"
        print $ parseString (some tknCharA) mempty "a ba b"
        print $ parseString (some tknCharA) mempty "a b a b"
        -- ^ stops at first "a b" because parser doesn't say
        -- anything about a space after b and the tokenizing
        -- behaviour only applies to what follow a
        let tknBoth =
                token (char 'a') >> token (char 'b')
        print $ parseString (some tknBoth) mempty "a b a b"
