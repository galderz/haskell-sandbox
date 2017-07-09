import Data.Functor.Identity


edgelordMap f t =
    runIdentity $ traverse (Identity . f) t


main :: IO ()
main =
    do  print $ traverse (Identity . (+1)) [1, 2]
        print $ runIdentity $ traverse (Identity . (+1)) [1, 2]
        print $ edgelordMap (+1) [1..5]
