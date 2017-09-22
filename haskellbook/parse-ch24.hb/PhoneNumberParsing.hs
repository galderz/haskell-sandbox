import Control.Applicative
import Data.Char (digitToInt)
import Text.Trifecta

-- aka area code
type NumberingPlanArea =
    Int


type Exchange =
    Int


type LineNumber =
    Int


data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)


parseSeparator :: Parser ()
parseSeparator =
    try $ skipMany (oneOf "-")
    <|> return ()


parseThreeDigits :: Parser Int
parseThreeDigits =
    do  n1 <- digit
        n2 <- digit
        n3 <- digit
        return $ stringToInt (n1 : n2 : n3 : [])


stringToInt :: String -> Int
stringToInt s =
    foldl (\z x -> (z * 10) + digitToInt x) 0 s


parsePhone :: Parser PhoneNumber
parsePhone =
    do  skipMany (oneOf "(")
        area <- parseThreeDigits
        skipMany (oneOf ") ")
        parseSeparator
        exchange <- parseThreeDigits
        parseSeparator
        number <- decimal
        return $ PhoneNumber area exchange (fromInteger number)


main :: IO ()
main =
    do  print $ parseString parsePhone mempty "123-456-7890"
        -- Success (PhoneNumber 123 456 7890)

        print $ parseString parsePhone mempty "1234567890"
        -- Success (PhoneNumber 123 456 7890

        print $ parseString parsePhone mempty "(123) 456-7890"
        --  Success (PhoneNumber 123 456 7890)
