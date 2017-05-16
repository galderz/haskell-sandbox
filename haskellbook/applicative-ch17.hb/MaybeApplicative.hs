
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

main :: IO ()
main =
    do  print $ validateLength 5 "basel"
        print $ validateLength 2 "basel"
