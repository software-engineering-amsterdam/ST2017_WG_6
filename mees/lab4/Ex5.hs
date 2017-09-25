module Ex5 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck
import SetOrd

type Rel a = [(a,a)]

-- aas :: Rel a
-- aas = [(1,2)]

inverseSet :: Rel a -> Rel a
inverseSet = map swap

symClos :: Ord a => Rel a -> Rel a
symClos x = nub (union x (inverseSet x))

-- (list2set x)
-- (list2set (inverseSet [(1,2),(3,4)]))



-- [(1,2),(3,4)]
-- unionSet (list2set x) (list2set (inverseSet x))


-- list2set (inverseSet [(1,2),(3,4)])
-- [(1,2),(3,4)]