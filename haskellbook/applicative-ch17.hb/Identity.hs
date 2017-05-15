import Control.Applicative


main :: IO ()
main =
    do  print $ const <$> [1, 2, 3] <*> [9, 9, 9]
        print $ const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
