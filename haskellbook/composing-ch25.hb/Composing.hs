{-# LANGUAGE InstanceSigs #-}


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


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x =
        Compose $ (pure . pure) x

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) =
        Compose $ (fmap (<*>) f) <*> a


-- impossible.
-- instance (Monad f, Monad g) => Monad (Compose f g) where
--     return =
--         pure

--     (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b (>>=) =
--         undefined


instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) =
        (foldMap . foldMap) f fga


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
