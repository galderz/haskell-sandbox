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
