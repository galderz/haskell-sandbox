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


instance Monad (Moi s) where
    return =
        pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g =
        Moi $ \s ->
                  let
                      (a, s') =
                          f s
                      m =
                          g a
                  in
                      runMoi m s'


get :: Moi s s
get =
    Moi $ \s -> (s, s)


put :: s -> Moi s ()
put s =
    Moi $ \_ -> ((), s)


exec :: Moi s a -> s -> s
exec (Moi sa) s =
    snd $ sa s


eval :: Moi s a -> s -> a
eval (Moi sa) s =
    fst $ sa s


modify :: (s -> s) -> Moi s ()
modify f =
    Moi $ \s -> ((), f s)


main :: IO ()
main =
    do  print $ (1, 0) == runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
        print $ ("curryIsAmaze", "curryIsAmaze") == runMoi get "curryIsAmaze"
        print $ ((), "blah") == runMoi (put "blah") "woot"
        print $ "wilma" == exec (put "wilma") "daphne"
        print $ "scooby papu" == exec get "scooby papu"
        print $ "bunnicula" == eval get "bunnicula"
        print $ "stake a bunny" == eval get "stake a bunny"
        print $ ((), 1) == runMoi (modify (+1)) 0
        print $ ((), 2) == runMoi (modify (+1) >> modify (+1)) 0
