module Ex2 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd
import Lab4

randSet :: IO (Set Int)
randSet = fmap list2set genIntList

instance (Integral a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = fmap list2set arbitrary
