import qualified Data.Map as M
import Data.Monoid


f :: M.Map Char Int
f =
    M.fromList [('a', 1)]


g :: M.Map Char Int
g =
    M.fromList [('b', 2)]


main :: IO ()
main =
    do  print $ f <> g
        print $ mappend f g
        print $ f `mappend` g
        -- but note what happens here:
        print $ M.fromList [('a', 1)] <> M.fromList [('a', 2)]
