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


parsePhone :: Parser PhoneNumber
parsePhone =
    undefined


main :: IO ()
main =
    do  print $ parseString parsePhone mempty "123-456-7890"
        -- Success (PhoneNumber 123 456 7890)
