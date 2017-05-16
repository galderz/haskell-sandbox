
newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)


instance Functor (Constant a) where
    fmap _ (Constant x) =
        Constant x


main :: IO ()
main =
    do  print $ fmap (+1) (Constant 5)
