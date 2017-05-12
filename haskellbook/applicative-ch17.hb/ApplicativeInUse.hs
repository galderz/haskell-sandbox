import Control.Applicative
import Data.Char


f x =
    lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]


g y =
    lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]


h z =
    lookup z [(2,3),(5,6),(7,8)]


m x =
    lookup x [(4, 10), (8, 13), (1, 9001)]


main :: IO()
main =
    let
        c (x:xs) = toUpper x:xs
    in
        do  print $ fmap (2^) [1,2,3]
            print $ fmap (^2) [1,2,3]
            print $ [(+1), (*2)] <*> [2, 4]
            -- print $ fmap (,) [1]
            print $ fmap (+1) (1, 2)
            print $ (,) <$> [1, 2] <*> [3, 4] --  [(1, ), (2, )] <*> [3, 4]
            print $ liftA2 (,) [1,2] [3,4]
            print $ (+) <$> [1, 2] <*> [3, 5]
            print $ liftA2 (+) [1,2] [3,5]
            print $ max <$> [1,2] <*> [1,4]
            print $ liftA2 max [1,2] [1,4]
            -- lookup
            -- lookup::Eqa=>a->[(a,b)]->Maybeb
            print $ lookup 3 [(3, "hello")]
            print $ fmap length $ lookup 3 [(3, "hello")]
            print $ fmap c $ lookup 3 [(3, "hello")]
            -- print $ fmap c $ Data.Map.lookup 3 (fromList [(3, "hello")])
            print $ f 3
            print $ g 8
            print $ (++) <$> f 3 <*> g 7
            print $ (+) <$> h 5 <*> m 1
            print $ liftA2 (++) (g 9) (f 4)
            print $ liftA2 (^) (h 5) (m 4)
            print $ liftA2 (*) (h 5) (m 4)
            print $ liftA2 (*) (h 1) (m 1)
