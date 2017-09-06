import Text.Trifecta

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
