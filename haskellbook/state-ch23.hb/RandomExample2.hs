module RandomExample2 where


import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random


-- Six-sided die
data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)


intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        -- Use this tactic _extremely_ sparingly.
        x -> error $ "intToDie got non 1-6 integer: " ++ show x


rollDie :: State StdGen Die
rollDie =
    state $
    do
        (n, s) <- randomR (1, 6)
        return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' =
    intToDie <$> state (randomR (1, 6))


-- Example of usage:
-- evalState rollDieThreeTimes' (mkStdGen 0)
rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie


-- Example of usage:
-- take 6 $ evalState infiniteDie (mkStdGen 0)
-- ^ Not correct since all values printed out are same
-- since it's using same seed in every call.
infiniteDie :: State StdGen [Die]
infiniteDie =
    repeat <$> rollDie


-- Example of usage:
-- evalState (nDie 5) (mkStdGen 0)
nDie :: Int -> State StdGen [Die]
nDie n =
    replicateM n rollDie



-- Example of usage:
-- rollsToGetTwenty (mkStdGen 0)
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g =
    go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 =
                  count
            | otherwise =
                  let (die, nextGen) =
                          randomR (1, 6) gen
                  in go (sum + die) (count + 1) nextGen
