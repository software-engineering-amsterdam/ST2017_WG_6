{--
Assignment:		Lab 4: Assignment 9
Name:           Sara Oonk
Time spent:
Sources:        Sangam's version at commit 686045631a716daa04abc8aa1c3b9a9f675f2163
                https://stackoverflow.com/questions/14006707/making-a-read-instance-in-haskell
                https://stackoverflow.com/questions/38815948/how-to-implement-a-read-instance-with-the-actual-parsing-already-complete-in-ha

--}

module Exc9 where

import Data.List
import Lecture4

{-------------------------------------------------------------------------------------------------------------------------------------
9)  Bonus In the lecture notes, Statement is in class Show, but the show function
    for it is a bit clumsy. Write your own show function for imperative programs.
    Next, write a read function, and use show and read to state some abstract test properties
    for how these functions should behave. Next, use QuickCheck to test your implementations.
    Deliverable: implementation, QuickCheck properties, test report.
--------------------------------------------------------------------------------------------------------------------------------------}

-- Example data--------------
expr :: Expr
expr = I 5

cdt1, cdt2 :: Condition
cdt1 = Eq (I 1) (I 1)
cdt2 = Eq (I 2) (I 2)
cdt5 = Ng (Eq (I 1) (I 2))

cdt3 = Dj[cdt1, cdt2]
cdt4 = Cj[cdt1, cdt2, cdt5]

assmt :: Statement
assmt = Ass "v" (V "v")

stmt :: Statement
stmt =  While cdt4 assmt
-----------------------------

showLst,showRest :: [Condition] -> String -> String
showLst [] _ = ""
showLst (c:cs) s = show c ++ showRest cs s
showRest [] _ = ""
showRest (c:cs) s = s ++ show c ++ showRest cs s

showSts :: [Statement] -> String
showSts [] = ""
showSts (s:ss) = show s ++ "; " ++ showSts ss

instance Show Expr where
    show (I i) = show i
    show (V v) = show v
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Subtr e1 e2) = show e1 ++ " - " ++ show e2
    show (Mult e1 e2) = show e1 ++ " * " ++ show e2

instance Show Condition where
    show (Eq e1 e2) = show e1 ++ " == " ++ show e2
    show (Lt e1 e2) = show e1 ++ " < " ++ show e2
    show (Gt e1 e2) = show e1 ++ " > " ++ show e2
    show (Ng c) = "!(" ++ show c ++ ")"
    show (Cj cs) = "(" ++ showLst cs " && " ++ ")"
    show (Dj cs) = "(" ++ showLst cs " || " ++ ")"

instance Show Statement where
    show (Ass v a) = v ++ " = " ++ show a
    show (Cond c s1 s2) = "if ( " ++ show c ++" ) then " ++ show s1 ++ " else " ++ show s2
    show (Seq ss) = showSts ss
    show (While c s) = "While " ++ show c ++ " do " ++ show s
