myLiftA2 :: Applicative f =>
    (a -> b -> c)
    -> f a -> f b -> f c
myLiftA2 f fa fb =
    f <$> fa <*> fb


newtype Reader r a =
    Reader { runReader :: r -> a }


asks :: (r -> a) -> Reader r a
asks f =
    Reader f
