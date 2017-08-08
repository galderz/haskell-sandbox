module ReaderPractice where

import Control.Applicative
import Data.Maybe


x =
    [1, 2, 3]


y =
    [4, 5, 6]


z =
    [7, 8, 9]


lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup =
    undefined


-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs =
    undefined


-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys =
    undefined



-- it's also nice to have one tha
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs =
    undefined


-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n =
    undefined
