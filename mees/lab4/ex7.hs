module Ex7 where

import Data.List
import Lecture4
import Test.QuickCheck
import Lab4
import SetOrd
import Ex5
import Ex6
import Ex2


-- prop :: Ord a => Set (a, a) -> Bool
-- prop (Set x) = trClos(x) == trClos(trClos(x))


propSubset :: (Integral a) => Rel a -> Bool
propSubset x = null ((nub x) \\ trClos(x))