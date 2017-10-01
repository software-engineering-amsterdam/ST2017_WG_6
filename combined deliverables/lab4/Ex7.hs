-- ################################################
-- # Mees Kalf
-- # Excersize 7
-- # 2 hour(s)
-- ################################################
module Ex7 where

import Data.List
import Lecture4
import Test.QuickCheck
import Lab4
import SetOrd
import Ex5
import Ex6
import Ex2

-- Properties Transitive Closure
-- Relation R should be a subset of the transitive closure of R
propSubset, propDecomp :: Rel Int -> Bool
propSubset x = null ((nub x) \\ trClos(x))

-- The trClo of R should be the same as the decomposition of R with its trClo
propDecomp x = trClos x  == sortUniq (union x (x @@ (trClos x)))

-- Properties Symetric closure
-- Relation R should a subset of Symetric closure of R


test7 :: (Rel Int -> Bool) -> Int -> IO ()
test7 f 0 = print ("++ All self-automated tests passed")
test7 f n = do
    x <- genIntList
    y <- genIntList
    if f (zip x y)
    then test7 f (n - 1)
    else error ("failed test on: " ++ show x ++ " AND " ++ show y)

main = do
    test7 propSubset 100
    test7 propDecomp 100

    quickCheck propDecomp
    quickCheck propSubset