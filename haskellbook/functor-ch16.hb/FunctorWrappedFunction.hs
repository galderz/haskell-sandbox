module FunctorFunction where

data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)


instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) =
        Wrap (fmap f fa)


main :: IO ()
main =
    do  print $ fmap (+1) (Wrap (Just 1))
        print $ fmap (+1) (Wrap [1, 2, 3])
        -- let n = 1 :: Integer
        -- print $ fmap (+1) (Wrap n)
