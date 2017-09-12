-- ##########
-- # https://blackboard.uva.nl/bbcswebdav/pid-6839934-dt-content-rid-11209217_1/courses/2318N001.5364SOTE6Y.S1.1.2017/Lab2.html
-- # Exercise Recognizing and generating derangements
-- ##########

import Data.List
import System.Random
import Test.QuickCheck
import Lecture2

isDerangement, checkProperty :: [Int]->[Int]-> Bool
isDerangement x y
    | (sort x) /= (sort y) = False
    | x == [] = False
    | otherwise = checkProperty x y

checkProperty [] [] = True
checkProperty (x:xs) (y:ys) 
    | x == y = False
    | otherwise = checkProperty xs ys

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n]) (permutations [0..n])

-- #########################################
-- #                                       #
-- #              Testing                  #
-- #                                       #
-- #########################################

-- 1. define some testable properties for the isDerangement function, 
{--
The isDerangement function checks if two lists are valid derangements of eachother 
by the definition as stated on Lab 2 from blackboard. A derangement B of list A,containing
only natural numbers, must serveral properties when the derangement suffices:

1 - Length A == Length B
2 - A is a permutation of B
3 - Commutativity: if A is a derangement of B then b is a derangement of A
4 - The reverse of A and B are also derangements of eachother


--}

prop_len :: [Int] -> [Int] -> Bool
prop_len x y = isDerangement x y --> length x == length y

prop_comm :: [Int] -> [Int] -> Bool
prop_comm x y = isDerangement x y --> isDerangement y x

prop_perm :: [Int] -> [Int] -> Bool
prop_perm x y = isDerangement x y --> sort x == sort y

prop_reverse :: [Int] -> [Int] -> Bool
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


-- ##########
-- # Helping functions
-- ##########

-- https://gist.github.com/kaveet/2fec32c18a35a51476711a912ff442c9
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverse xs ++ [x]

