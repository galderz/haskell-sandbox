import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)


data Optional a =
    Nada
    | Yep a
    deriving (Eq, Ord, Show)


data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Ord, Show)


instance Functor Identity where
    fmap f (Identity x) =
        Identity (f x)


instance Functor (Constant a) where
    fmap _ (Constant a) =
        Constant a


instance Functor Optional where
    fmap _ Nada =
        Nada
    fmap f (Yep x) =
        Yep (f x)


instance Functor List where
    fmap _ Nil =
        Nil
    fmap f (Cons x l) =
        Cons (f x) (fmap f l)


instance Foldable Identity where
    foldr f z (Identity x) =
        f x z

    foldMap f (Identity x) =
        f x


instance Foldable (Constant a) where
    foldr f z (Constant a) =
        z

    foldMap f (Constant a) =
        mempty


instance Foldable Optional where
    foldr _ z Nada =
        z
    foldr f z (Yep x) =
        f x z

    foldMap _ Nada =
        mempty
    foldMap f (Yep x) =
        f x


instance Foldable List where
    foldr _ z Nil =
        z
    foldr f z (Cons x l) =
        foldr f (f x z) l

    foldMap _ Nil =
        mempty
    foldMap f (Cons x l) =
        mappend (f x) (foldMap f l)


instance Traversable Identity where
    traverse f (Identity x) =
        fmap Identity (f x)


instance Traversable (Constant a) where
    traverse _ (Constant a) =
        pure (Constant a)


instance Traversable Optional where
    traverse _ Nada =
        pure Nada
    traverse f (Yep x) =
        fmap Yep (f x)


instance Traversable List where
    traverse _ Nil =
        pure Nil
    traverse f (Cons x l) =
        (fmap Cons (f x)) <*> (traverse f l)


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do  x <- arbitrary
            return $ Identity x


instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary =
        do  x <- arbitrary
            return $ Constant x


instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary =
        do  x <- arbitrary
            oneof [ return Nada
                  , return $ Yep x]


instance Arbitrary a => Arbitrary (List a) where
    arbitrary =
        sized arbList


-- Not yet know what "<*>" does exactly, but it's neat :)
arbList :: Arbitrary a => Int -> Gen (List a)
arbList 0 =
    return Nil
arbList n =
    frequency [
        (1, return Nil)
        , (4, fmap Cons arbitrary <*> (arbList (n - 1)))
    ]


instance (Eq a) => EqProp (Identity a) where
    (=-=) =
        eq


instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) =
        eq


instance (Eq a) => EqProp (Optional a) where
    (=-=) =
        eq


instance (Eq a) => EqProp (List a) where
    (=-=) =
        eq


type TI =
    Identity


type TC =
    Constant


type TO =
    Optional


type TL =
    List


main :: IO ()
main =
    do  let
            ti =
                undefined :: TI (Int, Int, [Int])
            tc =
                undefined :: TC (Int, Int, [Int]) (Int, Int, [Int])
            to =
                undefined :: TO (Int, Int, [Int])
            tl =
                undefined :: List (Int, Int, [Int])
        -- quickBatch (functor ti)
        -- quickBatch (traversable ti)
        -- quickBatch (functor tc)
        -- quickBatch (traversable tc)
        -- quickBatch (functor to)
        -- quickBatch (traversable to)
        -- quickBatch (functor tl)
        quickBatch (traversable tl)
