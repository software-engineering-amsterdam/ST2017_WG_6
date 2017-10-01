-- ################################################
-- # Mees Kalf
-- # Excersize 6
-- # 2 hour(s)
-- ################################################
module Ex6 where

import Data.List
import Lecture4
import Lab4

-- The transative closure or R contains al possible relation of R, or more
-- formally the transitive closure of R is the composition on itself until
-- the composition on itself is itself (no more extra relations can be composed
-- given R).
trClos :: Ord a => Rel a -> Rel a
trClos x = fp catComps x
           where catComps x = sortUniq $ union x (x @@ x)
