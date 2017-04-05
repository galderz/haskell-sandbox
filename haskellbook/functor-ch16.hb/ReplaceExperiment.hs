module ReplaceExperiment where


replaceWithP :: b -> Char
replaceWithP =
    const 'p'


lms :: [Maybe [Char]]
lms =
    [Just "Ave", Nothing, Just "woohoo"]


replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' =
    replaceWithP


liftedReplace :: Functor f => f a -> f Char
liftedReplace =
    fmap replaceWithP


-- The [] around Char is the f of f Char
-- The f of `f a` is the outermost [] in [Maybe [Char]]
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' =
    liftedReplace



-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
--   ::(Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted =
    (fmap . fmap) replaceWithP


-- f ~ []
-- f1 ~ Maybe
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' =
    twiceLifted


-- Prelude> :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP
--   :: (Functor f2, Functor f1, Functor f)=>
--      f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted :: (Functor f2, Functor f1, Functor f) =>
    f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted =
    (fmap . fmap . fmap) replaceWithP


-- f ~ []
-- f1 ~ Maybe
-- f2 ~ []
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' =
    thriceLifted


main :: IO ()
main =
    do  putStr "replaceWithP' lms:   "
        print (replaceWithP' lms)

        putStr "liftedReplace lms:   "
        print (liftedReplace lms)

        putStr "liftedReplace' lms:   "
        print (liftedReplace' lms)

        putStr "twiceLifted lms:   "
        print (twiceLifted lms)

        putStr "twiceLifted' lms:   "
        print (twiceLifted' lms)

        putStr "thriceLifted lms:   "
        print (thriceLifted lms)

        putStr "thriceLifted' lms:   "
        print (thriceLifted' lms)
