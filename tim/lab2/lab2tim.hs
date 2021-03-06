{-
Assignment:		Lab 2: ...
Name:           Tim Nederveen
UVA id:      	11959037
Remarks:        --
Sources:        --
-}

module Lab2Tim where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck  

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


{-------------------------------------------------------------------------------------------------------------------------------------
2.) Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:
- Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
- Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
- Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
- Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,
- Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

Deliverables: Haskell program, concise test report, indication of time spent.
--------------------------------------------------------------------------------------------------------------------------------------}


data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape 
triangle x y z = orderedInputTriangle (sort [x, y, z])

orderedInputTriangle :: [Integer] -> Shape 
orderedInputTriangle [x,y,z]
    | x + y <= z = NoTriangle
    | x == y && y == z = Equilateral
    | x^2 + y^2 == z^2 = Rectangular
    | x == y || y == z = Isosceles
    | otherwise = Other


inputs = [ [x,y,z] | x <- [-1..5], 
                     y <- [x..5], 
                     z <- [y..5] ]


-- test1 =  | [x,y,z] <- inputs
-- test2 =  | [x,z,z] <- 

-- TODO: mees zijn manualTests implemenation in exRAGD.hs

orderIrrelevant :: Integer -> Integer -> Integer -> Bool
orderIrrelevant = \x y z -> (triangle x y z == triangle y z x && triangle x y z == triangle z y x)

multiplicationIrrelevant :: Integer -> Integer -> Integer -> Integer -> Bool
multiplicationIrrelevant = \w x y z -> (triangle x y z == triangle (w*x) (w*y) (w*z))

test1 = quickCheckResult (\x y z -> (x >= 0 && y >= 0 && z >= 0) --> orderIrrelevant x y z)

test2 = quickCheckResult (\w x y z -> (w > 0 && x >= 0 && y >= 0 && z >= 0) --> multiplicationIrrelevant w x y z)



{-------------------------------------------------------------------------------------------------------------------------------------
3b.) Recognizing Permutations

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

--------------------------------------------------------------------------------------------------------------------------------------}


-- perms from Workshop 1 -------------------------------
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
--------------------------------------------------------

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` (perms ys)

-- Property: Identity permutation. Strong: Only one possible permutation
prop_identity :: Eq a => [a] -> Bool
prop_identity xs = isPermutation xs xs

-- -- Property: Identity permutation. Strong: Only one possible permutation
-- prop_identity :: Eq a => [a] -> [a] -> Bool
-- prop_identity xs ys = (isPermutation xs ys) == (isPermutation ys xs)

-- Property: Reversal permutation. Strong: Only one possible permutation
prop_reversal :: Eq a => [a] -> [a] -> Bool
prop_reversal xs ys = (isPermutation xs ys) == (isPermutation xs (reverse ys))

-- Property: Sorting permutation. Strong: Only one possible permutation
prop_sort_1 :: Ord a => [a] -> [a] -> Bool
prop_sort_1 xs ys = (isPermutation xs ys) == (isPermutation xs (sort ys))

-- Property: Sorting permutation. Strong: Only one possible permutation
prop_sort_2 :: Ord a => [a] -> [a] -> Bool
prop_sort_2 xs ys = (isPermutation xs ys) == (isPermutation (sort xs) ys)


-- Property: Cyclic permutation / Transposition / Shift. Weaker: Check all possible cyclic permutations

-- Modified function rotate by user 'dave4420' on May 4 '13 at https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
shift :: [a] -> [a]
shift [] = []
shift xs = zipWith const (drop 1 (cycle xs)) xs
--------------------------------------------------

prop_transposition :: Eq a => [a] -> [a] -> Bool
prop_transposition xs ys = (isPermutation xs ys) == (checkFullCycle xs ys (length(ys)))

checkFullCycle :: Eq a => [a] -> [a] -> Int -> Bool
checkFullCycle xs ys 0 = False
checkFullCycle xs ys n | (shift xs) == ys = True
                       | otherwise = checkFullCycle (shift xs) ys (n-1)

-- Property: Same format list. Weakest permutation test:
-- Simply check if lengths of arguments are equal AND all elements of A are present in B assuming elements are unique
prop_format :: Eq a => [a] -> [a] -> Bool
prop_format xs ys = (length(xs) == length(ys)) && (checkContainsAllElements xs ys)

checkContainsAllElements :: Eq a => [a] -> [a] -> Bool
checkContainsAllElements [] ys = True
checkContainsAllElements (x:xs) ys = x `elem` ys && checkContainsAllElements xs ys

