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
