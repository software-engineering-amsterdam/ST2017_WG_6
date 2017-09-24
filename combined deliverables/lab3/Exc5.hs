{-
Assignment:		Lab 3: Assignment 5
Name:           Sangam Gupta
Time spent:     3,5 hours
---------------}
module Exc5 where

import Data.List
import System.Random
import Lecture3
import Exc1
import Exc3
import Exc4

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[-x]] 
cnf2cls (Dsj xs) = [concat (concatMap cnf2cls xs)]
cnf2cls (Cnj xs) = concatMap cnf2cls xs

-- By transforming back to CNF we should get the same formula as the original one
cls2cnf :: Clauses -> Form
cls2cnf [[x]] | x > 0 = Prop x
              | otherwise = Neg (Prop (abs x))
cls2cnf [x:xs] = Dsj (cls2cnf [[x]] : [cls2cnf [xs]])
cls2cnf (x:xs) = Cnj (cls2cnf [x] : [cls2cnf xs])

-- This test fucntion and the one from Exc4 can be made generic

testExc5 :: Form -> Bool
testExc5 x = equiv (convertToCNF x) (cls2cnf  (cnf2cls (convertToCNF x)))

{-- All 100 testcases succeed. We test by converting cnf to cls and then back to cnf.
    By converting it back we expect the statement to be equivelant to the original.
    This test is valid because we know that cnf2cls produces a valid (syntactic) Clauses due to 
    the function type cnf2cls :: Form -> Clauses. By converting the cls form back to cnf in a
    different function written for the test. We can conclude with the equivalance check that the (semantics) is also correct.

--}
main = testm testExc5 1 100