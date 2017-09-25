module Ex6 where

import Data.List
import Lecture4
import Lab4

trClos :: Ord a => Rel a -> Rel a
trClos x = fp helper x
           where helper x = sort(nub(x ++ (x @@ x)))
