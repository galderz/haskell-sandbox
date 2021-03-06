
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s


newtype Name =
    Name String
    deriving (Eq, Show)


newtype Address =
    Address String
    deriving (Eq, Show)


mkName :: String -> Maybe Name
mkName s =
    fmap Name $ validateLength 25 s


mkAddress :: String -> Maybe Address
mkAddress a =
    fmap Address $ validateLength 100 a


data Person =
    Person Name Address
    deriving (Eq, Show)


mkPerson' :: String -> String -> Maybe Person
mkPerson' n a =
    case mkName n of
        Nothing ->
            Nothing
        Just n' ->
            case mkAddress a of
                Nothing ->
                    Nothing
                Just a' ->
                    Just $ Person n' a'


mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    Person <$> mkName n <*> mkAddress a


maybeAddy =
    mkAddress "old macdonald's"


main :: IO ()
main =
    do  print $ validateLength 5 "basel"
        print $ validateLength 2 "basel"
        print $ validateLength 10 "basel"
        print $ (fmap Person (mkName "Babe")) <*> maybeAddy
        print $ Person <$> mkName "Babe" <*> maybeAddy
