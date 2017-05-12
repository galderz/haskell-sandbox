import Data.Monoid


main :: IO ()
main =
    do  print $ [(*2), (*3)] <*> [4, 5] -- [2 * 4, 2 * 5, 3 * 4, 3 * 5]
        print $ Just (*2) <*> Just 2 -- Just 4
        print $ Just (*2) <*> Nothing -- Nothing
        -- Nothing <*> Just (*2) -- Nothing
        -- Nothing <*> Nothing -- Nothing
        print $ fmap (+1) ("blah", 1)
        -- No function applied for `a`, they've been joined through Monoid
        -- The function in `b` position has been applied
        -- to the element in right of tuple
        print $ ("Woo", (+1)) <*> (" Hoo!", 0)
        print $ ((Sum 2), (+1)) <*> ((Sum 0), 0)
        print $ ((Product 3), (+9)) <*> ((Product 2), 8)
        print $ ((All True), (+1)) <*> ((All False), 0)
