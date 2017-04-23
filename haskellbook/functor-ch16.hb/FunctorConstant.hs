module FunctorConstant where


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)


instance Functor (Constant m) where
    fmap _ (Constant v) =
        Constant v


main :: IO ()
main =
    do  print $ const 1 2
        print $ Constant 2
        print $ const 2 (getConstant (Constant 3))
        print $ fmap (const 2) (Constant 3)
        print $ getConstant $ fmap (const 2) (Constant 3)
        print $ fmap (const "blah") (Constant 3)
        -- id functor laws
        print $ getConstant (id (Constant 3))
        print $ getConstant (fmap id (Constant 3))
        -- composition laws
        print $ getConstant $ (fmap (const 3) . fmap (const 5)) $ (Constant "WOOHOO")
        print $ getConstant $ (fmap ((const 3) . (const 5))) $ (Constant "Dogs rule")
