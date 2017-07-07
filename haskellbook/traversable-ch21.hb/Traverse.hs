f =
    undefined :: a -> Maybe b


xs =
    undefined :: [a]


main :: IO ()
main =
    do  print $ fmap Just [1, 2, 3]
        print $ sequenceA $ fmap Just [1, 2, 3]
        print $ sequenceA . fmap Just $ [1, 2, 3]
        print $ traverse Just [1, 2, 3]
        -- :t map f xs
        -- map f xs :: [Maybe b]
        --
        -- What if we want a value of type Maybe [b] ?
        -- The following will work, but we can do better:
        --
        -- :t sequenceA $ map f xs
        -- sequenceA $ map f xs :: Maybe [a]
        --
        -- It's usually better to use travese
        -- whenever we see a sequence or sequenceA
        -- combined with a map or fmap:
        --
        -- :t traverse f xs
        -- traverse f xs :: Maybe [b]
