
{--
Assignment:		Lab 4: Assignment 2
Name:           Sangam Gupta
Time spent:     1 hour
--}
module Exc2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture4
import Lecture2

{-------------------------------------------------------------------------------------------------------------------------------------
2)  Implement a random data generator for the datatype Set Int, 
    where Set is as defined in SetOrd.hs. First do this from scratch, 
    next give a version that uses QuickCheck to random test this datatype.
    (Deliverables: two random test generators, indication of time spent.)
--------------------------------------------------------------------------------------------------------------------------------------}

-- Generate a random set of ints and convert that to a set
randomSetGenerator :: IO (Set Int)
randomSetGenerator = list2set <$> genIntList

-- Make Set a subclass of arbitrary. Quick then recognises data Set and knows 
-- -- How to use it.
instance (Integral a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary
