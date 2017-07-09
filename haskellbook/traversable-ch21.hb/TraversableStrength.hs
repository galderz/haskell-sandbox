import Data.Functor.Constant
import Data.Functor.Identity
import Data.Monoid


edgelordMap f t =
    runIdentity $ traverse (Identity . f) t


xs =
    [1, 2, 3, 4, 5] :: [Sum Integer]



-- Compared to:
-- foldMap :: (Foldable t, Monoid m)  => (a -> m) -> t a -> m
foldMap' :: (Monoid m, Traversable t) => (a -> m) -> t a -> m
foldMap' f t =
    getConstant $ traverse (Constant . f) t


main :: IO ()
main =
    do  print $ traverse (Identity . (+1)) [1, 2]
        print $ runIdentity $ traverse (Identity . (+1)) [1, 2]
        print $ edgelordMap (+1) [1..5]
        print $ traverse (Constant . (+1)) xs
