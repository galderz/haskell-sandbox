module FmapMaybe where


incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) =
    Just $ n + 1
incIfJust Nothing =
    Nothing


showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) =
    Just $ show s
showIfJust Nothing =
    Nothing


incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m =
    fmap (+1) m


showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s =
    fmap show s


main :: IO ()
main =
    do  print $ incIfJust (Just 1) -- Just 2
        print $ incIfJust Nothing -- Nothing
        print $ incMaybe (Just 1) -- Just 2
        print $ incMaybe Nothing -- Nothing
        print $ showMaybe (Just 9001) -- Just "9001"
        print $ showMaybe (Nothing :: Maybe String) -- Nothing
