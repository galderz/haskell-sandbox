-- you'll need this pragma
{-# LANGUAGE InstanceSigs #-}


newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) =
        undefined


main :: IO ()
main =
    do  print $ (1, 0) == runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
