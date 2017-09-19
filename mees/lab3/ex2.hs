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

parseTest :: Form -> Bool
parseTest f = show (head (parse (show f))) == show f

