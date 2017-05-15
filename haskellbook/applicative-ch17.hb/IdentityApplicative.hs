import Control.Applicative


newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)


instance Functor Identity where
    fmap f (Identity a)=
        Identity (f a)


instance Applicative Identity where
    pure =
        Identity
    (<*>) (Identity f) (Identity x) =
        Identity (f x)

main :: IO ()
main =
    do  print $ const <$> [1, 2, 3] <*> [9, 9, 9]
        print $ const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
