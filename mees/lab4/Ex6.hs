module Ex6 where

import Data.List
import Lab4
import Data.Tuple
import Lecture4
import SetOrd

trClos :: Ord a => Rel a -> Rel a
trClos x = sort(helper x x)
    where
        helper x [] = []
        helper x y = y ++ (helper x (x @@ y))

-- Second (also valid) solution with use of fix
trClos' :: Ord a => Rel a -> Rel a
trClos' x = sort ((x,x) $$ fix (\ f (x,y) -> if y == [] 
                                             then [] 
                                             else y ++ (f (x,(x @@ y)))))

-- trClos'' :: Ord a => Rel a -> Rel a
-- trClos'' x = until (y == []) (y ++ (x @@ y))

