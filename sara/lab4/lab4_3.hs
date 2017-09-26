{-
    Assignment:		Lab 4: Exercise 3 - Implementing and testing set operations
    Name:           Sara Oonk
    Time spent:     2h 30m
    Sources:        SetOrd.hs
                    Lecture2
                    http://web.mnstate.edu/peil/MDEV102/U1/S3/Property6.htm
                    http://www.mathcaptain.com/algebra/set-difference.html
                    http://planetmath.org/setdifference
                    http://www.math-only-math.com/intersection-of-sets.html
                    http://www.webovations.com/education/mathbook/book/sets7.htm
-}
module Exc3 where
import SetOrd
import Lecture2
import Data.List
import Test.QuickCheck

{-
    Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs.
    Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
    (Deliverables: implementations, test properties, short test report, indication of time spent.)

    unionSet is given in SetOrd.hs:
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   | unionSet :: (Ord a) => Set a -> Set a -> Set a                      |
     unionSet (Set [])     set2  =  set2
   | unionSet (Set (x:xs)) set2  = insertSet x (unionSet (Set xs) set2)  |
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-}


intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set [])     set2 = (Set [])
intersectionSet (Set (x:xs)) set2 = if (inSet x set2) then insertSet x (intersectionSet (Set xs) set2)
                                    else intersectionSet (Set xs) set2
                                    {- if x in both sets, keep x by appending it to recursive result -}
                                    {- otherwise discard x by NOT appending it to recursive result -}

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set [])     set2 = (Set [])
differenceSet (Set (x:xs)) set2 = if (inSet x set2) then differenceSet (Set xs) set2
                                  else insertSet x (differenceSet (Set xs) set2)
                                  {- if x in both sets, discard x by NOT appending it to recursive result -}
                                  {- otherwise keep x by appending it to recursive result -}

main = do
 automatedTests
 putStrLn "\n"
 quickChecks
 putStrLn "\n"
 unionTests
 putStrLn "\n"
 intersectionTests
 putStrLn "\n"
 differenceTests



automatedTests = do
 putStrLn "\n**** Automated tests for implementation ****"
 putStrLn "\n*** Intersection: ***"
 testSet testIntersection
 putStrLn "\n*** Difference: ***"
 testSet testDifference
 putStrLn "\n*** Union: ***"
 testSet testUnion

quickChecks = do
 putStrLn "\n**** QuickCheck tests for implementation ****"
 putStrLn "\n*** Intersection: ***"
 quickCheckSet testIntersection
 putStrLn "\n*** Difference: ***"
 quickCheckSet testDifference
 putStrLn "\n*** Union: ***"
 quickCheckSet testUnion

unionTests = do
 putStrLn "\n**** Automated test for union properties ****"
 putStrLn "\n*** Commutative law: ***"
 testSet commutativeUnion
 putStrLn "\n*** Law of identity element: ***"
 testSet identityUnion
 putStrLn "\n*** Idempotent law: ***"
 testSet idempotentUnion
 
intersectionTests = do
 putStrLn "\n**** Automated test for intersection properties ****"
 putStrLn "\n*** Commutative law: ***"
 testSet commutativeIntersection
 putStrLn "\n*** Law of identity element: ***"
 testSet identityIntersection
 putStrLn "\n*** Idempotent law: ***"
 testSet idempotentIntersection
 
differenceTests = do
 putStrLn "\n**** Automated test for difference properties ****"
 putStrLn "\n*** Commutative law: ***"
 testSet commutativeDifference
 putStrLn "\n*** Law of identity element: ***"
 testSet identityDifference
 putStrLn "\n*** Idempotent law: ***"
 testSet idempotentDifference


-- Automated tests
testSet :: ([Int] -> [Int] -> Bool) -> IO ()
testSet c = testS 1 100 c

testS :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testS k n c = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList
                  ys <- genIntList
                  if (c (nub $ sort xs) (nub $ sort ys)) then
                       testS (k+1) n c
                  else error ("failed test on: " ++ show (nub $ sort xs) ++ " with " ++ show (nub $ sort ys))


-- Test implementations using Haskell's Data.List operations
testIntersection, testDifference, testUnion :: [Int] -> [Int] -> Bool
testIntersection xs ys = (intersectionSet (Set xs) (Set ys)) == list2set (intersect xs ys)
testDifference   xs ys = (differenceSet (Set xs) (Set ys))   == list2set (xs \\ ys)
testUnion        xs ys = (unionSet (Set xs) (Set ys))        == list2set (xs `union` ys)

-- QuickCheck using above properties for 'setOp'
quickCheckSet :: ([Int] -> [Int] -> Bool) -> IO Result
quickCheckSet setOp = quickCheckResult (\xs -> \ys -> setOp (nub $ sort xs) (nub $ sort ys))

-- Properties for union
commutativeUnion, identityUnion, idempotentUnion :: [Int] -> [Int] -> Bool
commutativeUnion xs ys = (unionSet (Set xs) (Set ys)) == (unionSet (Set ys) (Set xs))
identityUnion    xs ys = (unionSet (Set xs) (Set [])) == (Set xs)
idempotentUnion  xs ys = (unionSet (Set xs) (Set xs)) == (Set xs)

-- Properties for intersection
commutativeIntersection, identityIntersection, idempotentIntersection :: [Int] -> [Int] -> Bool
commutativeIntersection xs ys = (intersectionSet (Set xs) (Set ys)) == (intersectionSet (Set ys) (Set xs))
identityIntersection    xs ys = (intersectionSet (Set xs) (Set [])) == (Set [])
idempotentIntersection  xs ys = (intersectionSet (Set xs) (Set xs)) == (Set xs)

-- Properties for difference
commutativeDifference, identityDifference, idempotentDifference :: [Int] -> [Int] -> Bool
commutativeDifference xs ys = xs /= ys --> (differenceSet (Set xs) (Set ys)) /= (differenceSet (Set ys) (Set xs))
identityDifference    xs ys = (differenceSet (Set xs) (Set [])) == (Set xs)
idempotentDifference  xs ys = (differenceSet (Set xs) (Set xs)) == (Set [])


