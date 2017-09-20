import Text.Trifecta


parseDigit :: Parser Char
parseDigit =
    oneOf "1234567890"


base10Integer :: Parser Integer
base10Integer =
    fmap (\s -> read s) (some parseDigit)


main :: IO ()
main =
    do
        print $ parseString parseDigit mempty "123"
        -- Success '1'

        print $ parseString parseDigit mempty "abc"
        -- Failure (interactive):1:1: error: expected: parseDigit
        -- abc<EOF>

        print $ parseString base10Integer mempty "123abc"
        -- Success 123

        -- print $ parseString base10Integer mempty "abc"
        -- Failure (interactive):1:1: error: expected: integer
        -- abc<EOF>
