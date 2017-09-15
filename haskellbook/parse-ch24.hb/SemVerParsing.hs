import Control.Applicative
import Data.Monoid
import Text.Trifecta

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.


data NumberOrString =
    NOSS String
    | NOSI Integer
    deriving (Eq, Show, Ord)


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


-- instance Ord NumberOrString where
--     compare (NOSS s) (NOSS s') = compare s s'
--     compare (NOIS n) (NOIS n') = compare n n'


instance Ord SemVer where
    compare (SemVer ma1 mi1 p1 r1 _) (SemVer ma2 mi2 p2 r2 _) =
        let c = (compare ma1 ma2)
                <> (compare mi1 mi2)
                <> (compare p1 p2)
        in case c of
            EQ ->
                compare r1 r2
            _ ->
                c


parseSemVer :: Parser SemVer
parseSemVer =
    do  major <- decimal
        char '.'
        minor <- decimal
        char '.'
        patch <- decimal
        release <- parseRelease
        metadata <- parseMetadata
        return $ SemVer major minor patch release metadata


parseRelease :: Parser Release
parseRelease =
    (char '-' >> many parseNosDot)
    <|> return []


parseMetadata :: Parser Metadata
parseMetadata =
    (char '+' >> many parseNosDot)
    <|> return []


parseNosDot :: Parser NumberOrString
parseNosDot =
    do  nos <- parseNos
        skipMany (oneOf ".")
        return nos


parseNos :: Parser NumberOrString
parseNos =
    (NOSS <$> some letter)
    <|> (NOSI <$> decimal)
    -- don't use integer here since it gets confused with +


main :: IO ()
main =
    do
        print $ parseString parseSemVer mempty "2.1.1"
        -- Success (SemVer 2 1 1 [] [])

        print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
        -- Success (SemVer 1 0 0
        --      [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

        print $ parseString parseSemVer mempty "1.0.0-alpha"
        -- Success (SemVer 1 0 0 [NOSS "alpha"] [])

        print $ parseString parseSemVer mempty "1.0.0-alpha+001"
        -- Success (SemVer 1 0 0 [NOSS "alpha"] [NOSI 1])

        print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
        -- True

        -- print $ SemVer 1 0 0 [NOSS "alpha"] []
        --     < SemVer 1 0 0 [] []
        -- True
