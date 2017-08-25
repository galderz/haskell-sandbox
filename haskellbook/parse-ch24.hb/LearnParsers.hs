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
