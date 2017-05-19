import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


trigger =
    undefined :: [(String, String, Int)]


main :: IO ()
main =
    do  quickBatch $ applicative [("b", "w", 1 :: Int)]
        quickBatch $ applicative trigger
