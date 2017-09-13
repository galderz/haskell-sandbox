import Text.Trifecta

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.


data NumberOrString =
    NOSS String
    | NOSI Integer
    deriving (Eq, Show)


type Major =
    Integer


type Minor =
    Integer


type Patch =
    Integer


type Release =
    [NumberOrString]


type Metadata =
    [NumberOrString]


data SemVer =
    SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show)


parseSemVer :: Parser SemVer
parseSemVer =
    do  major <- decimal
        char '.'
        minor <- decimal
        char '.'
        patch <- decimal
        return $ SemVer major minor patch [] []


main :: IO ()
main =
    do
        print $ parseString parseSemVer mempty "2.1.1"
        -- Success (SemVer 2 1 1 [] [])

        print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
        -- Success (SemVer 1 0 0
        --      [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

        -- print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
        --
