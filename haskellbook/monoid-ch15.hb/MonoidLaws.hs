module MonoidLaws where

import Data.Monoid


leftIdentitySum =
    mappend mempty (Sum 1) == (Sum 1)


rightIdentitySum =
    mappend (Sum 1) mempty == (Sum 1)


associativitySum =
    mappend (Sum 1) (mappend (Sum 2) (Sum 3)) ==
        mappend (mappend (Sum 1) (Sum 2)) (Sum 3)


associativitySum' =
    (Sum 1) <> (Sum 2 <> Sum 3) ==
        (Sum 1 <> Sum 2) <> Sum 3


concatSum =
    mconcat [Sum 1, Sum 2, Sum 3] ==
        foldr mappend mempty [Sum 1, Sum 2, Sum 3]


-- lists: mempty is []
-- lists: mappend is (++)

leftIdentityList =
    mappend mempty [1, 2, 3] == [1, 2, 3]
