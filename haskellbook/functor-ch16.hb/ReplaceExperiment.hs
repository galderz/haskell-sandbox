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
