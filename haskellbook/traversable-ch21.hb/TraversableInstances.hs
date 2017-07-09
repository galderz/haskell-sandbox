import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)


instance Functor Identity where
    fmap f (Identity x) =
        Identity (f x)


instance Foldable Identity where
    foldr =
        undefined

    foldMap =
        undefined


instance Traversable Identity where
    traverse =
        undefined


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do  x <- arbitrary
            return $ Identity x


instance (Eq a) => EqProp (Identity a) where
    (=-=) =
        eq


type TI = Identity


main :: IO ()
main =
    do  let trigger = undefined :: TI (Int, Int, [Int])
        quickBatch (functor trigger)
