{-
Assignment:		Lab 3: Assignment 1
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

{-------------------------------------------------------------------------------------------------------------------------------------
Exercise Implementing and testing IBAN validation

a)  The lecture notes of this week discuss the notions of satisfiability, tautology, contradiction, logical entailment and logical equivalence for formulas of propositional logic.

The lecture notes give a definition of satisfiable, for objects of type Form.

Your task is to give definitions of:
 contradiction
 tautology
 entails
 equiv
--------------------------------------------------------------------------------------------------------------------------------------}


{--
    We test the implemented specifications by using propositions p q and r, and form1, form2, form3 as specified in Lecture3.hs. 
    For each function, input for at least one valid and one non-valid result is tested, 
    using expressions of which the expected outcome is known.
--}

contradiction :: Form -> Bool
contradiction f = not (satisfiable f) 

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)


-- | logical entailment: B logically entails A is true if and only if it is necessary that if all of the elements of B are true, then A is true.
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

-- | logical equivalence: Equivalence if A and B are always equivalent
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)


testCont   = and [contradiction (Neg form1),
                  not (contradiction (form2)),
                  contradiction (Neg form3)]

testTaut   = and [tautology form1,
                  not (tautology form2),
                  tautology form3]

testEntail = and [not (entails form1 form2),
                  entails form2 form3] 

testEquiv  = and [equiv (Impl (Neg q) (Neg p)) (Impl p q),
                  not (equiv (Impl q p) (Impl q r))]

manualTests
    | not testCont = print "Contradiction tests failed"
    | not testTaut = print "Tautology tests failed"
    | not testEntail = print "Entailment tests failed"
    | not testEquiv = print "Equivalence tests failed"
    | otherwise = print "++ All manual set test cases are valid"

