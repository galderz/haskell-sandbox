module FmapEither where


incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) =
    Right $ n + 1
incIfRight (Left e) =
    Left e


showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) =
    Right $ show s
showIfRight (Left e) =
    Left e


incEither :: Num a => Either e a -> Either e a
incEither m =
    fmap (+1) m


showEither :: Show a => Either e a -> Either e String
showEither s =
    fmap show s


incEither' :: Num a => Either e a -> Either e a
incEither' =
    fmap (+1)


showEither' :: Show a => Either e a -> Either e String
showEither' =
    fmap show


liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc =
    fmap (+1)


liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow =
    fmap show


data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)


instance Functor (Sum a) where
    fmap f (First a) =
        First a
    fmap f (Second b) =
        Second (f b)

-- Repl:
-- fmap (+1) (First "blah")
-- fmap (+1) (Second 1)
