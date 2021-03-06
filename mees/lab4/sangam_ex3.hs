{--
Assignment:     Lab 4: Assignment 3
Name:           Sangam Gupta
Time spent:     2 hour
--}
module Exc4 where
    
import Data.List
import Test.QuickCheck
import SetOrd
import Exc2
    
{-------------------------------------------------------------------------------------------------------------------------------------
2)  Implement operations for set intersection, set union and set difference, 
    for the datatype Set defined in SetOrd.hs. Next, use automated testing to check 
    that your implementation is correct. First use your own generator, next use QuickCheck.
    (Deliverables: implementations, test properties, short test report, indication of time spent.)
--------------------------------------------------------------------------------------------------------------------------------------}

-- Union of Sets
unionSet', intersectionSet, differenceSet :: Integral a => Set a -> Set a -> Set a
unionSet' (Set x) (Set y)       = list2set $ x `union` y
differenceSet (Set x) (Set y)   = list2set $ x \\ y
intersectionSet (Set x) (Set y) = list2set $ x `intersect` y

{-
We won't test the concrete implementation of the functions above. E.g. testing it 
against the given unionSet in SetOrds.hs (do both functions return the same result) 
because we assume that the library functions union, intersect and difference 
work as intented and because testing it that way won't garauntee that the properties 
of a Set still hold. We would merely check if the functions return the same result without checking 
whether the returned result is correct.

Instead we will test if the properties for a Set hold after union, intersect and difference. This
way we gaurantee the result is correct.

http://www.cs.odu.edu/~toida/nerzic/level-a/set/set_op_prop.html more properties can be found here
-}

-- Union, intersection [1,2] [2,3] is the same as Union, intersection [2,3] [1,2]
testCommutative :: Integral a => (Set a -> Set a -> Set a) -> Set a -> Set a -> Bool
testCommutative f a b = (f a b) == (f b a)

-- Union, intersection [1,2] [1,2] is the same as the identity [1,2] 
testIdempotent :: Integral a => (Set a -> Set a -> Set a) -> Set a -> Bool
testIdempotent f a = (f a a) == a

testUnion, testIntersection, testDifference :: Set Int -> Set Int -> Bool
testUnion a b = testIdempotent unionSet' a && testCommutative unionSet' a b
testIntersection a b = testIdempotent intersectionSet a && testCommutative intersectionSet a b
testDifference a b = differenceSet a emptySet == a && subSet (differenceSet a b) a 

testUnionWithGen, testIntersectionWithGen, testDifferenceWithGen :: IO ()
testUnionWithGen = test8 testUnion 100
testIntersectionWithGen = test8 testIntersection 100
testDifferenceWithGen = test8 testDifference 100


test8 :: (Set Int -> Set Int -> Bool) -> Int -> IO ()
test8 f 0 = print ("++ All self-automated tests passed")
test8 f n = do
    x <- randomSetGenerator
    y <- randomSetGenerator
    if testUnion x y
    then test8 f (n - 1)
    else error ("failed test on: " ++ show x ++ " AND " ++ show y)

main = do 
        testUnionWithGen
        testIntersectionWithGen
        testDifferenceWithGen

        quickCheck testUnion
        quickCheck testIntersection
        quickCheck testIntersection