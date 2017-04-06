module HeavyLifting where


-- [2]
a =
    fmap (+1) $ read "[1]" :: [Int]


-- Just ["Hi,lol","Hellolol"]
b =
    (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- c 1 => -2
c =
    (*2) . (\x -> x - 2)
