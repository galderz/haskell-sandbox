module DoSyntax where

import Control.Monad (join)


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


binding :: IO ()
binding =
    do  name <- getLine
        putStrLn name


binding' :: IO ()
binding' =
    getLine >>= putStrLn


f :: Functor f => f String -> f (IO ())
f x =
    putStrLn <$> x


g :: (String -> b) -> IO b
g x =
    x <$> getLine


-- getLine :: IO String
-- putStrLn :: String -> IO ()
-- <$> ~= fmap
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- (String -> IO ()) -> IO String -> IO (IO ())
-- Running this function asks for data but does not print it
h :: IO (IO ())
h =
    putStrLn <$> getLine


-- join merges the effects of getLine and putStrLn
-- into a single IO action.
-- This merged IO action performs the effects
-- in the order determined by the nesting of the IO actions.
h' :: IO ()
h' =
    join $ putStrLn <$> getLine


twoActions :: (IO (), IO ())
twoActions =
    (putStrLn "1", putStrLn "2")


main :: IO ()
main =
    do  fst twoActions
        snd twoActions
        -- We're able to evaluate IO actions multiple times!
        fst twoActions
