{-
Assignment:		Lab 3: Assignment 5
Name:           Sangam Gupta
Time spent:     2 hours
---------------}
module Exc5 where

import Data.List
import System.Random
import Lecture3
import Exc4
import Exc3

type Clause  = [Int]
type Clauses = [Clause]

a1 = Prop 2
b1 = Neg a1
c1 = Dsj [Prop 3, b1]
d1 = Cnj [Prop 4, c1]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[-x]] 
cnf2cls (Dsj xs) = [concat (concatMap cnf2cls xs)]
cnf2cls (Cnj xs) = concatMap cnf2cls xs


-- TODO add tests
quickTest = do x <- randomForm 1
               print x
               print ( convertToCNF x)
               print ( cnf2cls (convertToCNF x))