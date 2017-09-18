-- The lecture notes of this week discuss the conversion of Boolean formulas 
-- (formulas of propositional logic) into CNF form. The lecture notes also give a 
-- definition of a Haskell datatype for formulas of propositional logic, using lists for 
-- conjunctions and disjunctions. Your task is to write a Haskell program for converting 
-- formulas into CNF.

-- Deliverables: conversion program with documentation, indication of time spent.

-- ################################################
-- # Mees Kalf
-- # Excersize 3
-- # 0.5 hour(s)
-- ################################################

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- satisfible is just for example, it's from Lecture3.hs
satisfible :: Form -> Bool
satisfible f = any (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\ v -> not(evl v f)) (allVals f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Logical entailment 
entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

-- Logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)
