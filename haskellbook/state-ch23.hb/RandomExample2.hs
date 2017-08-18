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
