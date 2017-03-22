module MonoidMem where

import Data.Monoid
import Test.Hspec


newtype Mem s a =
    Mem {
        runMem :: s -> (a, s)
    }


instance Monoid a => Monoid (Mem s a) where
    mempty =
        Mem (\s -> (mempty, s))

    mappend (Mem f) (Mem g) =
        Mem (\s ->
            let
                (a, x) = f s
                (b, y) = g x
            in
                (a <> b, y)
        )


f' :: Mem Integer [Char]
f' =
    Mem $ \s -> ("hi", s + 1)


main :: IO ()
main =
    do  print $ runMem (f' <> mempty) 0
        print $ runMem (mempty <> f') 0
        print $ (runMem mempty 0 :: (String, Int))
        print $ runMem (f' <> mempty) 0 == runMem f' 0
        print $ runMem (mempty <> f') 0 == runMem f' 0
