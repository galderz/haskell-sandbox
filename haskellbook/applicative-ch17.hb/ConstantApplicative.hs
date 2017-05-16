import Control.Applicative
import Data.Monoid


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)


instance Functor (Constant a) where
    fmap _ (Constant x) =
        Constant x


instance Monoid a => Applicative (Constant a) where
    pure _ =
        Constant mempty
    (<*>) (Constant f) (Constant y) =
        Constant (mappend f y)


main :: IO ()
main =
    do  print $ fmap (+1) (Constant 5)
        print $ Constant (Sum 1) <*> Constant (Sum 2)
        -- print $ Constant undefined <*> Constant (Sum 2)
        print $ (pure 1 :: Constant String Int)
