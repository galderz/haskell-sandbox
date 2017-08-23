-- you'll need this pragma
{-# LANGUAGE InstanceSigs #-}


newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) =
        Moi $ \s ->
                  let (a, s') =
                          g s
                  in
                      (f a, s')

        -- Moi $ \s -> (f (fst (g s)), s)


instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a =
        Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) =
        Moi $ \s ->
                  let
                      (f', s') =
                          f s
                      (a, s'') =
                          g s'
                  in
                      (f' a, s'')


main :: IO ()
main =
    do  print $ (1, 0) == runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
