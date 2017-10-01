-- ################################################
-- # Mees Kalf
-- # Excersize 8
-- # 2 hour(s)
-- ################################################
module Ex8 where

import Data.List
import Lecture4
import Test.QuickCheck
import Lab4
import SetOrd
import Ex5
import Ex6

-- Create the transitive closure of the symmetric closure and the symmetric
-- closure of the transitive closure
trClosSymClos :: Ord a => Rel a -> Rel a
trClosSymClos x = trClos(symClos(x))

symClosTrClos :: Ord a => Rel a -> Rel a
symClosTrClos x = symClos(trClos(x))

-- Check if the trClosSymClos and symClosTrClos are commutative, this test
-- can be executed with quickCheck, we see it fails: they are not commutative.
-- Using verboseCheck gives us the imput on which it crashes.
test8 :: Integral a => Rel a -> Bool
test8 x = trClosSymClos x == symClosTrClos x

{------------------------------------------------------------------------------
    
Assignment 8
Example that illustrates the difference:

Take a simple relation: [(0,1)]

If we first take the transative closure we get:
[(0,1)]

Then calculating the symmetric closure gives:
[(0,1),(1,0)]

If we calculate them the other way around and first apply the symmetric closure
on [(0,1)] we get:
[(0,1),(1,0)]

And then calulate the transitive closure of [(0,1),(1,0)], we get:
[(0,0),(0,1),(1,0),(1,1)]

Since [(0,1),(1,0)] is NOT [(0,0),(0,1),(1,0),(1,1)] therefore they are not
commutative.
------------------------------------------------------------------------------}