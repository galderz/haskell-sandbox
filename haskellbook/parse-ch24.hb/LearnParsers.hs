module LearnParsers where


import Text.Trifecta


stop :: Parser a
stop =
    unexpected "stop"


-- read a single character '1'
one :: Parser Char
one =
    char '1'


-- read a single character '1', then die
-- equivalent to char '1' >> stop
--
-- (>>) :: Monad m => m a -> m b -> m b
-- use >> instead of >>= because result is being thrown away
one' :: Parser b
one' =
    one >> stop


-- read two characters, '1', and '2'
oneTwo :: Parser Char
oneTwo =
    char '1' >> char '2'


-- read two characters,
-- '1' and '2', then die
oneTwo' :: Parser b
oneTwo' =
    oneTwo >> stop


testParse :: Parser Char -> String -> IO ()
testParse p str =
    print $ parseString p mempty str


oneTwoEof :: Parser ()
oneTwoEof =
    oneTwo >> eof


pNL s =
    putStrLn ('\n' : s)


singleParser :: String -> Parser String
singleParser s =
    string s


stringParserWithChar :: String -> Parser Char
stringParserWithChar (y:ys) =
    foldr (\x z -> z >> char x) (char y) ys


stringParserWithChar' :: String -> Parser String
stringParserWithChar' s =
    -- foldr ((>>) . char) mempty s
    -- foldr (\x z -> (char x) >> z) (string "") s
    --
    -- traverse :: Applicative f =>
    -- (a -> f b)            -> t a      -> f      (t b)
    -- (char -> Parser Char) -> [String] -> Parser [String]
    traverse char s


main :: IO ()
main =
    do  pNL "stop:"
        testParse stop "123"
        pNL "one:"
        testParse one "123"
        pNL "one':"
        testParse one' "123"
        pNL "oneTwo:"
        testParse oneTwo "123"
        pNL "oneTwo':"
        testParse oneTwo' "123"
        pNL "oneTwoEof:"
        print $ parseString oneTwoEof mempty "123"
        pNL "12<eof>:"
        print $ parseString oneTwoEof mempty "12"
        pNL "1:"
        print $ parseString (singleParser "1") mempty "123"
        pNL "12:"
        print $ parseString (singleParser "12") mempty "123"
        pNL "123:"
        print $ parseString (singleParser "123") mempty "123"
        pNL "1<char>:"
        print $ parseString (stringParserWithChar "1") mempty "123"
        pNL "12<char>:"
        print $ parseString (stringParserWithChar "12") mempty "123"
        pNL "123<char>:"
        print $ parseString (stringParserWithChar "123") mempty "123"
        pNL "1<char>:"
        print $ parseString (stringParserWithChar' "1") mempty "123"
        pNL "12<char>:"
        print $ parseString (stringParserWithChar' "12") mempty "123"
        pNL "123<char>:"
        print $ parseString (stringParserWithChar' "123") mempty "123"
