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
import Data.String
import Lecture3
import Lecture2


randExpr 0 = do Prop <$> (getRandomInt 3)
randExpr num = do
    x <- getRandomInt 4
    p1 <- randExpr (num - 1)
    p2 <- randExpr (num - 1)
    case x of
        0-> do return (Neg p1)
        1-> do return (Cnj [p1, p2])
        2-> do return (Dsj [p1, p2])
        3-> do return (Impl p1 p2)
        4-> do return (Equiv p1 p2)


checkCNF :: Form ->  Bool
checkCNF f
    | arrowfree f /= f = False
    | nnf f /= f = False
    | otherwise = isCNF f
    where
        isCNF :: Form -> Bool
        isCNF (Cnj f) = all isCNF f
        isCNF (Dsj (f1:ft)) = (dsjCheck f1) && (isCNF (Dsj ft))
        isCNF f = True

        dsjCheck :: Form -> Bool
        dsjCheck (Cnj fs) = False
        dsjCheck (Dsj (f1:ft)) = (dsjCheck f1) && (dsjCheck (Dsj ft))
        dsjCheck x = True


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

simpleTest f = (checkCNF f, checkCNF (convertToCNF f))
