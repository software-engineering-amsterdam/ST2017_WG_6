{-
Assignment:		Lab 2: Exercise 5 - Recognizing and generating derangements
Name:           Mees Kalf
Time spent:     4h
Remarks:        See below.
Sources:        reverseList from https://gist.github.com/kaveet/2fec32c18a35a51476711a912ff442c9

---------------}

module Exc5Derangements where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture2

isDerangement :: [Int]->[Int]-> Bool
isDerangement [] [] = False
isDerangement x y
    | (sort x) /= (sort y) = False
    | or (zipWith (==) x y) = False
    | otherwise = True

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n - 1]) (permutations [0..n - 1])

-- #########################################
-- #                                       #
-- #              Testing                  #
-- #                                       #
-- #########################################

{--
The isDerangement function checks if two lists are valid derangements of eachother
by the definition as stated on Lab 2 from blackboard. A derangement B of list A,containing
only natural numbers, must serveral properties when the derangement suffices:

1 - Length A == Length B
2 - A is a permutation of B
3 - Commutativity: if A is a derangement of B then b is a derangement of A
4 - The reverse of A and B are also derangements of eachother

For these properties we conducted some automated test with quickcheck, but since
the possibility that two random Int lists created by quickcheck are a derangement
is quite small. Therefore we also conducted some manual tests with well chosen Int lists
to be sure that we preserve the robustness of our implemenation.
--}

-- Properties
prop_len, prop_comm, prop_perm, prop_reverse:: [Int] -> [Int] -> Bool
prop_len x y = isDerangement x y --> length x == length y
prop_comm x y = isDerangement x y --> isDerangement y x
prop_perm x y = isDerangement x y --> sort x == sort y
prop_reverse x y = isDerangement x y --> isDerangement (reverseList (x)) (reverseList (y))

manualTests
    | isDerangement [] [] = print "Test empty list FAILED"
    | isDerangement [1,2,3] [1,2,2] = print "Test double Int FAILED"
    | isDerangement [1,2,3] [1,2,3,4] = print "Test not equal length FAILED"
    | isDerangement [1,2,3] [3,2,1] = print "Test i in A and B same value FAILED"
    | otherwise = print "++ All manual set test cases are valid"

main = do
    print "Test same length A and B"
    quickCheck prop_len

    print "Test commutativety A and B"
    quickCheck prop_comm

    print "Test B and A are permutations of eachother"
    quickCheck prop_perm

    print "Test if reverse of A and B are valid derangements"
    quickCheck prop_reverse

    manualTests

{-
Although we where not able to to generate the stronger and weaker properties we
can still manually derive it. The preconditon that it is a derangement we 
have given in our properties, and given that no tests are failing, all properties
are equally strong within the space of a derangement, eq it is not possible
that the Int list is a derangement and the property fails on any property.

Therefore they are al stronger and weaker than eachother.
-}

-- ##########
-- # Helping functions
-- ##########

-- https://gist.github.com/kaveet/2fec32c18a35a51476711a912ff442c9
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverse xs ++ [x]