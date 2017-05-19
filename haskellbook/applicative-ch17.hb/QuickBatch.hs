import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


main :: IO ()
main =
    quickBatch $ applicative [("b", "w", 1 :: Int)]
