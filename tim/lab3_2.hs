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

parseTest :: Form -> Bool
parseTest f = head (parse (show f)) == f

manualTests = all parseTest [form1,form2,form3, Neg form2, Impl form2 form3, Cnj [form2, form1, form3], Dsj [form1,form3]]