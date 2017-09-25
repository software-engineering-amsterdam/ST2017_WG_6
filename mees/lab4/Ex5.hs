module Ex5 where

import Data.List
import Data.Tuple
import Lab4
import System.Random
import Test.QuickCheck
import SetOrd

inverseSet :: Rel a -> Rel a
inverseSet = map swap

symClos :: Ord a => Rel a -> Rel a
symClos x = nub (union x (inverseSet x))
