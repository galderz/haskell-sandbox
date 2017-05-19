import Data.Monoid


main :: IO ()
main =
    print $ 1 <> (2 :: Sum Integer)
