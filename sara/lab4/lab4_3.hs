{-
    Assignment:		Lab 4: Exercise 3 - Implementing and testing set operations
    Name:           Sara Oonk
    Time spent:     3h 30m
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
onionSet, intersectionSet, differenceSet :: (Ord a) => Set a -> Set a -> Set a
onionSet        (Set xs) (Set ys) = list2set (union xs ys)
intersectionSet (Set xs) (Set ys) = list2set (intersect xs ys)
differenceSet   (Set xs) (Set ys) = list2set (xs \\ ys)

main = do
 automatedTests
 putStrLn "\n"
 quickChecks
 putStrLn "\n"
 propertyTests

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

propertyTests = do
 putStrLn "\n**** Automated tests for relational properties ****"
 putStrLn "\n*** Commutative laws: ***"
 quickCheck (\xs -> \ys -> commutative onionSet        (nub $ sort xs) (nub $ sort ys))
 quickCheck (\xs -> \ys -> commutative intersectionSet (nub $ sort xs) (nub $ sort ys))
 quickCheck (\xs -> \ys -> (commutative differenceSet   (nub $ sort xs) (nub $ sort ys)) == False || xs == ys)

 putStrLn "\n*** Identity laws: ***"
 quickCheck (\xs -> identitySelf  onionSet        (nub $ sort xs))
 quickCheck (\xs -> identitySelf  differenceSet   (nub $ sort xs))
 quickCheck (\xs -> identityEmpty intersectionSet (nub $ sort xs))

 putStrLn "\n*** Idempotent laws: ***"
 quickCheck (\xs -> idempotent onionSet        (nub $ sort xs))
 quickCheck (\xs -> idempotent intersectionSet (nub $ sort xs))
 quickCheck (\xs -> (idempotent differenceSet  (nub $ sort xs)) == False || xs == [])

 putStrLn "\n*** Associative laws: ***"
 quickCheck (\xs -> \ys -> \zs -> associative onionSet        (nub $ sort xs) (nub $ sort ys) (nub $ sort zs))
 quickCheck (\xs -> \ys -> \zs -> associative intersectionSet (nub $ sort xs) (nub $ sort ys) (nub $ sort zs))

 putStrLn "\n*** Distributive laws: ***"
 quickCheck (\xs -> \ys -> \zs -> distributive onionSet intersectionSet (nub $ sort xs) (nub $ sort ys) (nub $ sort zs))
 quickCheck (\xs -> \ys -> \zs -> distributive intersectionSet onionSet (nub $ sort xs) (nub $ sort ys) (nub $ sort zs))


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


-- Test implementations using Haskell's Data.List vs own extensive implementation (see bottom of file)
testIntersection, testDifference, testUnion :: [Int] -> [Int] -> Bool
testIntersection xs ys = (intersectionSet (Set xs) (Set ys)) == intSet   (Set xs) (Set ys)
testDifference   xs ys = (differenceSet (Set xs) (Set ys))   == difSet   (Set xs) (Set ys)
testUnion        xs ys = (onionSet (Set xs) (Set ys))        == unionSet (Set xs) (Set ys)

-- QuickCheck using above properties for 'testOperation'
quickCheckSet :: ([Int] -> [Int] -> Bool) -> IO ()
quickCheckSet testOperation = quickCheck(\xs -> \ys -> testOperation (nub $ sort xs) (nub $ sort ys))


-- Testable relational properties on sets - using [Int] for easy quickcheck
idempotent, identitySelf, identityEmpty  :: (Set Int -> Set Int -> Set Int) -> [Int] -> Bool
idempotent    f xs = (f (Set xs) (Set xs)) == (Set xs)
identitySelf  f xs = (f (Set xs) (Set [])) == (Set xs)
identityEmpty f xs = (f (Set xs) (Set [])) == (Set [])

commutative :: (Set Int -> Set Int -> Set Int) -> [Int] -> [Int] -> Bool
commutative f xs ys = f (Set xs) (Set ys) == f (Set ys) (Set xs)

associative :: (Set Int -> Set Int -> Set Int) -> [Int] -> [Int] -> [Int] -> Bool
associative f xs ys zs = f (f (Set xs) (Set ys)) (Set zs) == f (Set xs) (f (Set ys) (Set zs))

distributive :: (Set Int -> Set Int -> Set Int) -> (Set Int -> Set Int -> Set Int) -> [Int] -> [Int] -> [Int] -> Bool
distributive f g xs ys zs = f (Set xs) (g (Set ys) (Set zs)) == g (f (Set xs) (Set ys)) (f (Set xs) (Set zs))



-- Extensive implementations of intersectionSet and differenceSet using SetOrd in a fashion similar to given unionSet
intSet :: (Ord a) => Set a -> Set a -> Set a
intSet (Set [])     set2 = (Set [])
intSet (Set (x:xs)) set2 = if (inSet x set2) then insertSet x (intSet (Set xs) set2)
                                    else intSet (Set xs) set2
                                    {- if x in both sets, keep x by appending it to recursive result -}
                                    {- otherwise discard x by NOT appending it to recursive result -}

difSet :: (Ord a) => Set a -> Set a -> Set a
difSet (Set [])     set2 = (Set [])
difSet (Set (x:xs)) set2 = if (inSet x set2) then difSet (Set xs) set2
                                  else insertSet x (difSet (Set xs) set2)
                                  {- if x in both sets, discard x by NOT appending it to recursive result -}
                                  {- otherwise keep x by appending it to recursive result -}


