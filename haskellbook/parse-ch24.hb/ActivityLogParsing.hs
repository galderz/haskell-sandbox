import Text.Trifecta


-- aka area code
type Day =
    String


type Activities =
    [Activity]


type Activity =
    (Time, Description)


type Time =
    Integer


type Description =
    String


type History =
    [(Day, Activities)]


data Log =
    Log History
    deriving (Eq, Show)
