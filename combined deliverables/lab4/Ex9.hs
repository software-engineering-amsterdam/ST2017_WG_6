{--
Assignment:		Lab 4: Assignment 9
Name:           Sangam Gupta
Time spent:     2h 30m
--}

module Ex9 where
    
import Data.List
import Lecture4

{-------------------------------------------------------------------------------------------------------------------------------------
9)  Bonus In the lecture notes, Statement is in class Show, but the show function 
    for it is a bit clumsy. Write your own show function for imperative programs. 
    Next, write a read function, and use show and read to state some abstract test properties 
    for how these functions should behave. Next, use QuickCheck to test your implementations.
    Deliverable: implementation, QuickCheck properties, test report.


    There is no read implemented yet, after some research it seems like a parser such as in
    Lecture 3 is needed. The final test would at least test the property that showing then reading
    a statement should result in exactly the same statement:

    statement == read (show statement)
--------------------------------------------------------------------------------------------------------------------------------------}


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
    show (Ng c) = "!( " ++ show c ++ " )" 
    show (Cj cs) = "( " ++ showLst cs " && " ++ " )" 
    show (Dj cs) = "( " ++ showLst cs " || " ++ " )" 


instance Show Statement where
    show (Ass v a) = v ++ " = " ++ show a
    show (Cond c s1 s2) = "if ( " ++ show c ++" ) then " ++ show s1 ++ " else " ++ show s2
    show (Seq ss) = showSts ss
    show (While c s) = "While " ++ show c ++ " do " ++ show s