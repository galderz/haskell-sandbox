module FunctorLaws where


idLaw =
    fmap id "Hi Julie" == id "Hi Julie"


compositionLaw =
    fmap ((+1) . (*2)) [1..5] == (fmap (+1) . fmap (*2) $ [1..5])
