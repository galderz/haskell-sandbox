module Module where

import System.Random


main :: IO ()
main =
    do  print $ mkStdGen 0
        let sg = mkStdGen 0
        print $ next sg
        print $ next sg
