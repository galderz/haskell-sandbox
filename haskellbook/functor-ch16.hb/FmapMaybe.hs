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


incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' =
    fmap (+1)


showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' =
    fmap show


liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc =
    fmap (+1)


liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow =
    fmap show


data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)


instance Functor Possibly where
    fmap f LolNope =
        LolNope
    fmap f (Yeppers a) =
        Yeppers (f a)


main :: IO ()
main =
    do  print $ incIfJust (Just 1) -- Just 2
        print $ incIfJust Nothing -- Nothing
        print $ incMaybe (Just 1) -- Just 2
        print $ incMaybe Nothing -- Nothing
        print $ showMaybe (Just 9001) -- Just "9001"
        print $ showMaybe (Nothing :: Maybe String) -- Nothing
        print $ liftedInc (Just 1) -- Just 2
        print $ liftedInc Nothing -- Nothing
        print $ liftedShow (Just 1) -- Just "1"
        print $ liftedShow (Nothing :: Maybe String) -- Nothing
        print $ liftedInc [1..5]
        print $ liftedShow [1..5]
        print $ fmap (+1) LolNope
        print $ fmap (+1) (Yeppers 1)
