-- The lecture notes of this week discuss the conversion of Boolean formulas 
-- (formulas of propositional logic) into CNF form. The lecture notes also give a 
-- definition of a Haskell datatype for formulas of propositional logic, using lists for 
-- conjunctions and disjunctions. Your task is to write a Haskell program for converting 
-- formulas into CNF.

-- Deliverables: conversion program with documentation, indication of time spent.

-- ################################################
-- # Mees Kalf
-- # Excersize 3
-- # X hour(s)
-- ################################################

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

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
        disLaw (Cnj (f11:f12)) f2 = Cnj [(disLaw f11 f2), (disLaw (Cnj f12) f2)]
        disLaw f1 (Cnj (f21:f22)) = Cnj [(disLaw f1 f21), (disLaw f1 (Cnj f22))]
        disLaw f1 f2 = Dsj [f1, f2]

a = Prop 1
b = Prop 2
c = Prop 3
d = Prop 4

-- Should be (after cnf): (A ∧ ((B ∨ C) ∧ (B ∨ D)))
-- Result               : *(A *(+(B C) *(+(B D) +(B *()))))
form4 = Cnj [a, (Dsj [b, (Cnj [c,d])])]

-- Truth Table, origin vs converted
ttOriginVSConverted f = zipWith (==) (map (\ v -> evl v f) (allVals f)) (map (\ v -> evl v (convertToCNF f)) (allVals f))
