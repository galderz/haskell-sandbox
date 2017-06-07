module DoSyntax where

sequencing :: IO ()
sequencing =
    do  putStrLn "blah"
        putStrLn "another thing"


sequencing' :: IO()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"


sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"
