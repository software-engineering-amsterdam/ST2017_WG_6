-- ################################################
-- # Mees Kalf
-- # Excersize 2
-- # 1,5 hour(s)
-- ################################################
module Ex2 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lab4

-- We simply use the random list generator from Lecture2.hs and convert it to
-- a set with the list2set function given in SetOrd.hs for this week
randSet :: IO (Set Int)
randSet = fmap list2set genIntList

-- To be able to test our new set function with quickcheck we need to include
-- our set as a subclass of Arbitrary so quickcheck know how to generate
-- random test.
instance (Integral a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = fmap list2set arbitrary
