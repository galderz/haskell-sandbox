newtype Identity a =
    Identity { runIdentity :: a }


newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)


main :: IO ()
main =
  do  print $ Compose [Just 1, Nothing]
      let xs =
              [Just (1::Int), Nothing]
      -- Compose [] Maybe Int
      print $ Compose xs
