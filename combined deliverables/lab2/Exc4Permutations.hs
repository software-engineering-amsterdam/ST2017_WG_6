module Exc4Permutations where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

{-
    Recognizing Permutations

    A permutation of a finite list is another finite list with the same elements, but possibly in a different order.
    For example, [3,2,1] is a permutation of [1,2,3], but [2,2,0] is not.
    Write a function,   isPermutation :: Eq a => [a] -> [a] -> Bool   ,that returns True if its arguments are permutations of each other.

    Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation.
    You may assume that your input lists do not contain duplicates.
    What does this mean for your testing procedure?

--
"This means that I will assume that all list elements are unique, and I will assume that all valid permutations
 are of the same length as their original (un-permutated) version and contain all of (and only) those elements.
 This makes the testing procedure easier and less prone to errors, thus more reliable."
--

    Provide an ordered list of properties by strength using the weaker and stronger definitions.
    Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.

    Deliverables: Haskell program, concise test report, indication of time spent.


    Time spent: 4h

-}

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` (perms ys)

-- Property: Identity permutation. Strongest: Recognizes only one type of permutation
prop_identity :: [Char] -> [Char] -> Bool
prop_identity xs ys = xs == ys --> isPermutation xs ys

-- Property: Reversal permutation. Equally strongest: Recognizes only one type of permutation
prop_reversal :: [Char] -> [Char] -> Bool
prop_reversal xs ys = xs == (reverse ys) --> isPermutation xs ys

-- Property: Sorting permutation. Equally strongest: Recognizes only one type of permutation
prop_sort :: [Char] -> [Char] -> Bool
prop_sort xs ys = xs == (sort ys) --> isPermutation xs ys

-- Property: Cyclic permutation / Transposition / Shift. Weaker: Recognizes all cyclic permutations
prop_transposition :: [Char] -> [Char] -> Bool
prop_transposition xs ys = checkFullCycle xs ys (length(ys)) --> isPermutation xs ys

-- Property: Any permutation. Weakest permutation test: Recognizes all permutations on lists with unique elements
prop_format :: [Char] -> [Char] -> Bool
prop_format xs ys = ((length(xs) == length(ys)) && (checkContainsAllElements xs ys)) --> isPermutation xs ys



-------Tests--------
manualTests
    | False == isPermutation [1,2,3] [2,1,3] = print "Scrambled integers should be permutation: FAILED"
    | False == isPermutation ['a','b','c'] ['b','c','a'] = print "Scrambled letters should be permutation: FAILED"
    | False == isPermutation ['1','b','@'] ['@','1','b'] = print "Scrambled mixed characters should be permutation: FAILED"
    | isPermutation [1,2,3] [2,1] = print "Missing elements: FAILED"
    | isPermutation [1,2,3] [2,1,4] = print "Foreign elements: FAILED"
    | isPermutation [1,2,3] [] = print "Permutation is not empty list: FAILED"
    | isPermutation [1,2,3] [1,2,3,1,2,3] = print "Not equal length: FAILED"
    | isPermutation ['a', 'b', 'c'] ['a','a','a'] = print "Duplicate characters are not a permutation: FAILED"
    | otherwise = print "++ OK, All manual set test cases are valid"

main = do
    print "Strongest property: Identity permutation - Identical lists are permutation of each other"
    quickCheck prop_identity

    print "Equally strongest property: Reversal permutation - Reversing is a permutation"
    quickCheck prop_reversal

    print "Equally strongest property: Sorting permutation - Sorting is a permutation"
    quickCheck prop_sort

    print "Weaker property: Cyclic permutation - Rotating (shifting) a list is a permutation"
    quickCheck prop_transposition

    print "Weakest property: Any permutation based on equal length and presence of each element"
    quickCheck prop_format

    manualTests



--Helpers------------------------------------------------------------------------------------------------------
checkFullCycle :: [Char] -> [Char] -> Int -> Bool
checkFullCycle xs ys 0 = False
checkFullCycle xs ys n | (shift xs) == ys = True
                       | otherwise = checkFullCycle (shift xs) ys (n-1)

checkContainsAllElements :: [Char] -> [Char] -> Bool
checkContainsAllElements [] ys = True
checkContainsAllElements (x:xs) ys = x `elem` ys && checkContainsAllElements xs ys

-- Modified function rotate by user 'dave4420' on May 4 '13 at https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
shift :: [a] -> [a]
shift [] = []
shift xs = zipWith const (drop 1 (cycle xs)) xs
--------------------------------------------------

-- perms from Workshop 1 -------------------------------
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
--------------------------------------------------------
