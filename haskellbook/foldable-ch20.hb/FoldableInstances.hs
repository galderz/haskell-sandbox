import Data.Monoid


data Identity a =
    Identity a


data Optional a =
    Nada
    | Yep a
    deriving (Eq, Show)


-- The point of foldable for Identity is less to reduce
-- the values inside the structure to one value, and more
-- to consume or use the value.
instance Foldable Identity where
    foldr f z (Identity x) =
        f x z

    foldl f z (Identity x) =
        f z x

    foldMap f (Identity x) =
        f x


instance Foldable Optional where
    foldr _ z Nada =
        z
    foldr f z (Yep x) =
        f x z

    foldl _ z Nada =
        z
    foldl f z (Yep x) =
        f z x

    foldMap _ Nada =
        mempty
    foldMap f (Yep a) =
        f a


main :: IO ()
main =
    do  print $ foldr (*) 1 (Identity 5)
        print $ foldl (*) 5 (Identity 5)
        print (foldMap (*5) (Identity 100) :: Product Integer)
        print $ foldr (+) 1 Nothing
        print (foldMap (+1) Nothing :: Sum Integer)
        print $ foldr (+) 1 (Just 3)
        print (foldMap (+1) $ Just 3 :: Sum Integer)
        -- If you don't tell it what Monoid to use, it won't work
        -- print (foldMap (+1) Nada)
        print (foldMap (+1) Nada :: Sum Int)
        print (foldMap (+1) Nada :: Product Int)
        print (foldMap (+1) (Just 1) :: Sum Int)
