

main :: IO ()
main =
    do  print $ [(*2), (*3)] <*> [4, 5] -- [2 * 4, 2 * 5, 3 * 4, 3 * 5]
        print $ Just (*2) <*> Just 2 -- Just 4
        print $ Just (*2) <*> Nothing -- Nothing
        -- Nothing <*> Just (*2) -- Nothing
        -- Nothing <*> Nothing -- Nothing
