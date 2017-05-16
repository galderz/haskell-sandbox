
data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" =
    Nothing
noEmpty str =
    Just str


noNegative :: Int -> Maybe Int
noNegative n    | n >= 0 =
                    Just n
                | otherwise =
                    Nothing


-- Validating to get rid of empty strings, negative numbers
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
    case noEmpty name' of
        Nothing ->
            Nothing
        Just nammy ->
            case noNegative age' of
                Nothing ->
                    Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing ->
                            Nothing
                        Just weighty ->
                            Just (Cow nammy agey weighty)

main :: IO ()
main =
    undefined
