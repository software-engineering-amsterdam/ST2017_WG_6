module Ex8 where

import Data.List
import Lecture4
import Test.QuickCheck
import Lab4
import SetOrd
import Ex5
import Ex6

trClosSymClos :: Ord a => Rel a -> Rel a
trClosSymClos x = trClos(symClos(x))

symClosTrClos :: Ord a => Rel a -> Rel a
symClosTrClos x = symClos(trClos(x))

test8 :: Integral a => Rel a -> Bool
test8 x = trClosSymClos x == symClosTrClos x

{-------------------------------------------------------------------------------------
    
Take a simple relation: [(0,1)]

If we first take the transative closure we get:
[(0,1)]

Then calculating the symmetric closure gives:
[(0,1),(1,0)]

If we calculate then the other way around and first apply the symmetric closure:
[(0,1),(1,0)]

And then calulate the transitive closure of [(0,1),(1,0)], we get:
[(0,0),(0,1),(1,0),(1,1)]

Since [(0,1),(1,0)] is NOT [(0,0),(0,1),(1,0),(1,1)] therefore they are not
commutative.
--------------------------------------------------------------------------------------}