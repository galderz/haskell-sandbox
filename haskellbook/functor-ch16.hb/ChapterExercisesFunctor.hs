{-# LANGUAGE FlexibleInstances #-}

module ChapterExercisesFunctor where

import GHC.Arr
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen (oneof)


-- Can a valid Functor be provided?

-- 1. No, kind *
data Bool' =
   False | True


-- 2. Yes, kind * -> *
data BoolAndSomethingElse a =
    False' a | True' a


-- 3. Yes, kind * -> *
data BoolAndMaybeSomethingElse a =
    Falsish | Truish a


-- 4. Yes, kind * -> *
newtype Mu f =
    InF { outF :: f(Mu f) }


-- 5. No, kind is *
data D =
    D (Array Word Word) Int Int


data Sum b a =
    First a
    | Second b


instance Functor (Sum e) where
    fmap f (First a) =
        First (f a)
    fmap f (Second b) =
        Second b


data Company a c b =
    DeepBlue a c
    | Something b


instance Functor (Company e e') where
    fmap f (Something b) =
        Something (f b)
    fmap _ (DeepBlue a c) =
        DeepBlue a c


data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)


instance Functor (More x) where
    fmap f (L a b a') =
        L (f a) b (f a')
    fmap f (R b a b') =
        R b (f a) b'


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f =
    fmap id f == f


functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
    (fmap g (fmap f x)) == (fmap (g . f) x)


type StrToInt = Fun String Int


type IntToStr = Fun Int String


data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)


instance Functor (Quant a) where
    fmap f Finance =
        Finance
    fmap f (Desk a) =
        Desk a
    fmap f (Bloor b) =
        Bloor (f b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary =
        do  a <- arbitrary
            b <- arbitrary
            oneof [return $ Finance, return $ Desk a, return $ Bloor b]


type QuantFC = Quant Bool String -> StrToInt -> IntToStr -> Bool


data K a b =
    K a
    deriving (Eq, Show)


instance Functor (K a) where
    fmap f (K a) =
        K a


instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary =
        do  a <- arbitrary
            return $ K a


type KFC = K Bool String -> StrToInt -> IntToStr -> Bool


newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b =
    K' a
    deriving (Eq, Show)


-- should remind you of an
-- instance you've written before
instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) =
        Flip $ K' (f a)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K' a b) where
    arbitrary =
        do  a <- arbitrary
            return $ Flip (K' a)


type FlipK'FC = Flip K' Bool String -> StrToInt -> IntToStr -> Bool


data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)


instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) =
        GoatyConst (f b)


-- instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
--     arbitrary =
--         do  a <- arbitrary
--             b <- arbitrary
--             return $ GoatyConst b
--

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
    arbitrary =
        do  b <- arbitrary
            return $ GoatyConst b


type EvilGoatFC = EvilGoateeConst Char String -> StrToInt -> IntToStr -> Bool


data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)


instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) =
        LiftItOut (fmap f fa)


instance Arbitrary a => Arbitrary (LiftItOut Maybe a) where
    arbitrary =
        do  a <- arbitrary
            oneof [return $ LiftItOut (Just a), return $ LiftItOut Nothing]


type LiftItOutFC = LiftItOut Maybe String -> StrToInt -> IntToStr -> Bool


data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Eq, Show)


data Pair a =
    Pair a a
    deriving (Eq, Show)


instance Functor Pair where
    fmap f (Pair a a') =
        Pair (f a) (f a')


instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) =
        DaWrappa (fmap f fa) (fmap f ga)


instance Arbitrary a => Arbitrary (Parappa Maybe Pair a) where
    arbitrary =
        do  a <- arbitrary
            oneof [
                return $ DaWrappa (Just a) (Pair a a)
                , return $ DaWrappa Nothing (Pair a a)
                ]


type ParappaFC = Parappa Maybe Pair String -> StrToInt -> IntToStr -> Bool


data IgnoreOne f g a b =
    IgnoreSomething (f a) (g b)
    deriving (Eq, Show)


instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething a ga) =
        IgnoreSomething a (fmap f ga)


instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe Pair a b) where
    arbitrary =
        do  a <- arbitrary
            b <- arbitrary
            oneof [
                return $ IgnoreSomething (Just a) (Pair b b)
                , return $ IgnoreSomething Nothing (Pair b b)
                ]


type IgnoreOneFC = IgnoreOne Maybe Pair Char String -> StrToInt -> IntToStr -> Bool


data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)


instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious o a gt) =
        Notorious o a (fmap f gt)



instance (Arbitrary o, Arbitrary a, Arbitrary t) =>
    Arbitrary (Notorious Pair o a t) where
    arbitrary =
        do  o <- arbitrary
            a <- arbitrary
            t <- arbitrary
            return $ Notorious (Pair o o) (Pair a a) (Pair t t)


type NotoriousFC =
    Notorious Pair Bool Char String -> StrToInt -> IntToStr -> Bool


data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)


instance Functor List where
    fmap f Nil =
        Nil
    fmap f (Cons a l) =
        Cons (f a) (fmap f l)


-- Simple solution, only generates lists of one element max
-- instance Arbitrary a => Arbitrary (List a) where
--     arbitrary =
--         do  a <- arbitrary
--             oneof [
--                 return Nil
--                 , return $ Cons a Nil
--                 ]


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


type ListFC =
    List String -> StrToInt -> IntToStr -> Bool


data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)


instance Functor GoatLord where
    fmap f NoGoat =
        NoGoat
    fmap f (OneGoat a) =
        OneGoat (f a)
    fmap f (MoreGoats x y z) =
        MoreGoats (fmap f x) (fmap f y) (fmap f z)


instance Arbitrary a => Arbitrary (GoatLord a) where
    arbitrary =
        sized arbGoatLord


-- Not yet know what "<*>" does exactly, but it's neat :)
arbGoatLord :: Arbitrary a => Int -> Gen (GoatLord a)
arbGoatLord 0 =
    return NoGoat
arbGoatLord n =
    frequency [
        (1, return NoGoat)
        , (2, fmap OneGoat arbitrary)
        , (1, fmap MoreGoats arbitrary <*> arbitrary <*> (arbGoatLord (n - 1)))
        ]


type GoatLordFC =
    GoatLord String -> StrToInt -> IntToStr -> Bool


data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)
    -- deriving (Eq, Show)


instance (Show a) => Show (TalkToMe a) where
    show Halt =
        "Halt"
    show (Print s a) =
        "Print(" ++ s ++ "," ++ show a ++")"
    show (Read f) =
        "Read(f)"


instance (Eq a) => Eq (TalkToMe a) where
    Halt == Halt =
        Prelude.True
    (Print x a) == (Print y b) =
        (x == y) && (a == b)
    (Read f) == (Read g) =
        Prelude.True -- TODO: Hack!?


instance Functor TalkToMe where
    fmap f Halt =
        Halt
    fmap f (Print s a) =
        Print s (f a)
    fmap f (Read g) =
        -- Read (\s -> f (g s))
        Read (fmap f g)


instance Arbitrary a => Arbitrary (TalkToMe a) where
    arbitrary =
        do  a <- arbitrary
            s <- arbitrary
            oneof [
                return Halt
                , return (Print s a)
                , return (Read (\_ -> a))
                ]


type TalkToMeFC =
    TalkToMe String -> StrToInt -> IntToStr -> Bool


main :: IO ()
main =
    do  print $ fmap (+1) (L 1 2 3) -- L 2 2 4
        print $ fmap (+1) (R 1 2 3) -- R 1 3 3
        quickCheck $ \x -> functorIdentity (x :: Quant Bool String)
        quickCheck (functorCompose :: QuantFC)
        quickCheck $ \x -> functorIdentity (x :: K Bool String)
        quickCheck (functorCompose :: KFC)
        quickCheck $ \x -> functorIdentity (x :: Flip K' Bool String)
        quickCheck (functorCompose :: FlipK'FC)
        quickCheck $ \x -> functorIdentity (x :: EvilGoateeConst Char String)
        quickCheck (functorCompose :: EvilGoatFC)
        quickCheck $ \x -> functorIdentity (x :: LiftItOut Maybe String)
        quickCheck (functorCompose :: LiftItOutFC)
        quickCheck $ \x -> functorIdentity (x :: Parappa Maybe Pair String)
        quickCheck (functorCompose :: ParappaFC)
        quickCheck $ \x -> functorIdentity (x :: IgnoreOne Maybe Pair Char String)
        quickCheck (functorCompose :: IgnoreOneFC)
        quickCheck $ \x -> functorIdentity (x :: Notorious Pair Bool Char String)
        quickCheck (functorCompose :: NotoriousFC)
        quickCheck $ \x -> functorIdentity (x :: List String)
        quickCheck (functorCompose :: ListFC)
        quickCheck $ \x -> functorIdentity (x :: GoatLord String)
        quickCheck (functorCompose :: GoatLordFC)
        quickCheck $ \x -> functorIdentity (x :: TalkToMe String)
        quickCheck (functorCompose :: TalkToMeFC)
