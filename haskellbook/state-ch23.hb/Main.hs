module Module where

import System.Random


main :: IO ()
main =
    do  print $ mkStdGen 0
        let sg = mkStdGen 0
        print $ next sg
        print $ next sg
        print $ snd (next sg)
        let newSg = snd (next sg)
        print $ next newSg
        print $ (random newSg :: (Int, StdGen))
        print $ (random newSg :: (Double, StdGen))
        print $ (randomR (0, 3) newSg :: (Int, StdGen))
        print $ (randomR (0, 3) newSg :: (Double, StdGen))
        let rx :: (Int, StdGen); rx = random newSg
        print $ rx
        print $ snd rx
