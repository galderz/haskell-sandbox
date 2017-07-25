import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)


instance Functor Identity where
    fmap f (Identity x) =
        Identity (f x)


instance Functor (Constant a) where
    fmap _ (Constant a) =
        Constant a


instance Foldable Identity where
    foldr f z (Identity x) =
        f x z

    foldMap f (Identity x) =
        f x


instance Foldable (Constant a) where
    foldr =
        undefined

    foldMap =
        undefined


instance Traversable Identity where
    traverse f (Identity x) =
        fmap Identity (f x)


instance Traversable (Constant a) where
    traverse =
        undefined


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do  x <- arbitrary
            return $ Identity x


instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary =
        do  x <- arbitrary
            return $ Constant x


instance (Eq a) => EqProp (Identity a) where
    (=-=) =
        eq


instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) =
        eq


type TI = Identity


type TC = Constant


main :: IO ()
main =
    do  let
            ti =
                undefined :: TI (Int, Int, [Int])
            tc =
                undefined :: TC (Int, Int, [Int]) (Int, Int, [Int])
        -- quickBatch (functor ti)
        -- quickBatch (traversable ti)
        quickBatch (functor tc)
