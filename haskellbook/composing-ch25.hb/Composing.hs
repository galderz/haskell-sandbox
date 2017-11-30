newtype Identity a =
    Identity { runIdentity :: a }


newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) =
        Identity (f a)


instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga


v :: Compose [] Maybe (Compose Maybe [] Integer)
v =
    Compose [Just (Compose $ Just [1])]


main :: IO ()
main =
  do  print $ Compose [Just 1, Nothing]
      let xs =
              [Just (1::Int), Nothing]
      -- Compose [] Maybe Int
      print $ Compose xs
      print $ fmap (+1) (Compose xs)
      print $ v
