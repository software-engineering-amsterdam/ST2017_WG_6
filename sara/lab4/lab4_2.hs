{-
    Assignment:		Lab 4: Exercise 2
    Name:           Sara Oonk
    Time spent:     15m
    Sources:


    Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs.
    First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
    (Deliverables: two random test generators, indication of time spent.)
-}
module Exc2 where
import SetOrd
import Lecture2
import Data.List
import Test.QuickCheck

genRandomSet :: IO (Set Int)
genRandomSet = do
 xs <- genIntList
 return (list2set xs)


quickCheckRandomSet = quickCheck (\xs -> (testSet xs) == (list2set xs))

testSet :: [Int] -> Set Int
testSet xs = (list2set xs)
