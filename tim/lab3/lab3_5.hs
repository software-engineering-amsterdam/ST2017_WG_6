{-
Assignment:		Lab 3: Assignment 5
Name:           Tim Nederveen
Time spent:     TODO

Remarks:        - 

Sources:        - 
---------------}

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj xs) = map cnf2clssub xs
cnf2cls x = cnf2cls (Cnj [x])

cnf2clssub :: Form -> Clause
cnf2clssub (Prop x) = [x]
cnf2clssub (Neg (Prop x)) = [-x]
-- cnf2cls (Cnj []) = cnf2cls x : cnf
-- cnf2cls (Dsj [a]) =
-- cnf2cls (Dsj (f1:f2)) = 

    -- where
    --     toCLS :: Form -> Clauses
    --     [[4],[5,−6]]



convertToCNF :: Form ->  Form
convertToCNF = toCNF . nnf . arrowfree
    where
        toCNF :: Form -> Form
        toCNF (Prop x) = Prop x
        toCNF (Neg (Prop x)) = (Neg (Prop x))
        toCNF (Cnj fs) = Cnj (map toCNF fs)
        toCNF (Dsj [a]) = toCNF a
        toCNF (Dsj (f1:f2)) = disLaw (toCNF f1) (toCNF (Dsj f2))

        disLaw :: Form -> Form -> Form
        disLaw (Cnj []) _ = Cnj []
        disLaw (Cnj [f1]) f2 = disLaw f1 f2
        disLaw (Cnj (f11:f12)) f2 = Cnj [(disLaw f11 f2), (disLaw (Cnj f12) f2)]
        disLaw f1 (Cnj (f21:f22)) = Cnj [(disLaw f1 f21), (disLaw (Cnj f22) f1)]
        disLaw f1 f2 = Dsj [f1, f2]

a = Prop 1
b = Prop 2
c = Prop 3
d = Prop 4

-- Is (A /\ (B \/ (C /\ D)))
-- Should be (after cnf): (A ∧ ((B ∨ C) ∧ (B ∨ D)))
form4 = Cnj [a, (Dsj [b, (Cnj [c,d])])]

-- Truth Table, origin vs converted
ttOriginVSConverted f = zipWith (==) (map (\ v -> evl v f) (allVals f)) (map (\ v -> evl v (convertToCNF f)) (allVals f))

r12 = parse "+(+(-1 2))"