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


oneTwoEof :: Parser b
oneTwoEof =
    oneTwo >> eof >> stop


pNL s =
    putStrLn ('\n' : s)


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
        testParse oneTwoEof "123"
        pNL "12<eof>:"
        testParse oneTwoEof "12"
