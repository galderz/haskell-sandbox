module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
    fmap id f == f


functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
    (fmap g (fmap f x)) == (fmap (g . f) x)


newtype Identity a =
    Identity a
    deriving (Eq, Show)


data Pair a =
    Pair a a
    deriving (Eq, Show)


data Two a b =
    Two a b
    deriving (Eq, Show)


data Three a b c =
    Three a b c
    deriving (Eq, Show)


data Three' a b =
    Three' a b b
    deriving (Eq, Show)


data Four a b c d =
    Four a b c d
    deriving (Eq, Show)


data Four' a b =
    Four' a a a b
    deriving (Eq, Show)


data Trivial =
    Trivial
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) =
        Identity (f a)


instance Functor Pair where
    fmap f (Pair a a') =
        Pair (f a) (f a')


instance Functor (Two a) where
    fmap f (Two a b) =
        Two a (f b)


instance Functor (Three a b) where
    fmap f (Three a b c) =
        Three a b (f c)


instance Functor (Three' a) where
    fmap f (Three' a b b') =
        Three' a (f b) (f b')


instance Functor (Four a b c) where
    fmap f (Four a b c d) =
        Four a b c (f d)


instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) =
        Four' a a' a'' (f b)

-- • Expected kind ‘* -> *’, but ‘Trivial’ has kind ‘*’
-- So, no Functor can be defined for Trivial
-- instance Functor Trivial where
--     fmap f Trivial =
--         Trivial


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary =
        do  x <- arbitrary
            return $ Identity x


instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            return $ Pair x y


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            return $ Two x y


instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            z <- arbitrary
            return $ Three x y z


instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            z <- arbitrary
            return $ Three' x y z


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            z <- arbitrary
            z' <- arbitrary
            return $ Four x y z z'


instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary =
        do  x <- arbitrary
            y <- arbitrary
            z <- arbitrary
            z' <- arbitrary
            return $ Four' x y z z'


type StrToInt = Fun String Int


type IntToStr = Fun Int String


type IdFC = Identity String -> StrToInt -> IntToStr -> Bool


type PairFC = Pair String -> StrToInt -> IntToStr -> Bool


type TwoFC = Two Char String -> StrToInt -> IntToStr -> Bool


type ThreeFC = Three Bool Char String -> StrToInt -> IntToStr -> Bool


type ThreeFC' = Three' Char String -> StrToInt -> IntToStr -> Bool


type FourFC = Four Double Bool Char String -> StrToInt -> IntToStr -> Bool


type FourFC' = Four' Char String -> StrToInt -> IntToStr -> Bool


main :: IO ()
main =
    do  quickCheck $ \x -> functorIdentity (x :: Identity String)
        quickCheck (functorCompose :: IdFC)
        quickCheck $ \x -> functorIdentity (x :: Pair String)
        quickCheck (functorCompose :: PairFC)
        quickCheck $ \x -> functorIdentity (x :: Two Char String)
        quickCheck (functorCompose :: TwoFC)
        quickCheck $ \x -> functorIdentity (x :: Three Bool Char String)
        quickCheck (functorCompose :: ThreeFC)
        quickCheck $ \x -> functorIdentity (x :: Three' Char String)
        quickCheck (functorCompose :: ThreeFC')
        quickCheck $ \x -> functorIdentity (x :: Four Double Bool Char String)
        quickCheck (functorCompose :: FourFC)
        quickCheck $ \x -> functorIdentity (x :: Four' Char String)
        quickCheck (functorCompose :: FourFC')
