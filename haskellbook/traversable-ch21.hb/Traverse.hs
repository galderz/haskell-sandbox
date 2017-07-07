import Data.Map


f =
    undefined :: a -> Maybe b


xs =
    undefined :: [a]


type Morse = String


letterToMorse :: (Map Char Morse)
letterToMorse =
    Data.Map.fromList
        [ ('a', ".-")
        , ('b', "-...")
        , ('c', "-.-.")
        , ('d', "-..")
        , ('e', ".")
        , ('f', "..-.")
        , ('g', "--.")
        , ('h', "....")
        , ('i', "..")
        , ('j', ".---")
        , ('k', "-.-")
        , ('l', ".-..")
        , ('m', "--")
        , ('n', "-.")
        , ('o', "---")
        , ('p', ".--.")
        , ('q', "--.-")
        , ('r', ".-.")
        , ('s', "...")
        , ('t', "-")
        , ('u', "..-")
        , ('v', "...-")
        , ('w', ".--")
        , ('x', "-..-")
        , ('y', "-.--")
        , ('z', "--..")
        , ('1', ".----")
        , ('2', "..---")
        , ('3', "...--")
        , ('4', "....-")
        , ('5', ".....")
        , ('6', "-....")
        , ('7', "--...")
        , ('8', "---..")
        , ('9', "----.")
        , ('0', "-----")
        ]


charToMorse :: Char -> Maybe Morse
charToMorse c =
    Data.Map.lookup c letterToMorse


stringToMorse :: String -> Maybe [Morse]
stringToMorse s =
    -- sequence $ fmap charToMorse s
    traverse charToMorse s


morseToLetter :: Data.Map.Map Morse Char
morseToLetter =
    Data.Map.foldWithKey (flip Data.Map.insert) Data.Map.empty letterToMorse


morseToChar :: Morse -> Maybe Char
morseToChar m =
    Data.Map.lookup m morseToLetter


main :: IO ()
main =
    do  print $ fmap Just [1, 2, 3]
        print $ sequenceA $ fmap Just [1, 2, 3]
        print $ sequenceA . fmap Just $ [1, 2, 3]
        print $ traverse Just [1, 2, 3]
        -- :t map f xs
        -- map f xs :: [Maybe b]
        --
        -- What if we want a value of type Maybe [b] ?
        -- The following will work, but we can do better:
        --
        -- :t sequenceA $ map f xs
        -- sequenceA $ map f xs :: Maybe [a]
        --
        -- It's usually better to use travese
        -- whenever we see a sequence or sequenceA
        -- combined with a map or fmap:
        --
        -- :t traverse f xs
        -- traverse f xs :: Maybe [b]
        print $ morseToChar "gobbledegook"
        print $ morseToChar "-.-."
