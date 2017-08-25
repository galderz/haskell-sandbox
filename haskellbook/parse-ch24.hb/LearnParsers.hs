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


testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"


pNL s =
    putStrLn ('\n' : s)


main :: IO ()
main =
    do  pNL "stop:"
        testParse stop
        pNL "one:"
        testParse one
        pNL "one':"
        testParse one'
        pNL "oneTwo:"
        testParse oneTwo
        pNL "oneTwo':"
        testParse oneTwo'
