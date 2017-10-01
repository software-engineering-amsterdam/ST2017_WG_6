-- ################################################
-- # Mees Kalf
-- # Excersize 5
-- # 1,5 hour(s)
-- ################################################
module Ex5 where

import Data.List
import Data.Tuple
import Lab4
import Lecture4

-- As denoted on the wikipedia page : https://en.wikipedia.org/wiki/Symmetric_closure
-- the symetric closure of relation R is the union of R with its inverse relation R-
symClos :: Ord a => Rel a -> Rel a
symClos x = sortUniq $  union x (map swap x)
