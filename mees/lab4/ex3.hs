module Ex3 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lab4

unionSSet, intersectSet, diffSet :: (Ord a) => Set a -> Set a -> Set a
unionSSet    (Set x) (Set y) = list2set $ union x y
diffSet      (Set x) (Set y) = list2set $ x \\ y
intersectSet (Set x) (Set y) = list2set $ x \\ (x \\ y)