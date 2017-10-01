-- ################################################
-- # Mees Kalf
-- # Excersize 3
-- # 0.5 hour(s)
-- ################################################
module Ex3 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lab4

-- We read the set input as a list, then compute the union, difference
-- or intersect of these lists and convert them back to a set. Although
-- it is a kind of a dirty implentation it certainly works and is a short
-- and clean solution.
unionSSet, intersectSet, diffSet :: (Ord a) => Set a -> Set a -> Set a
unionSSet    (Set x) (Set y) = list2set $ union x y
diffSet      (Set x) (Set y) = list2set $ x \\ y
intersectSet (Set x) (Set y) = list2set $ x \\ (x \\ y)