{-
Assignment:		Lab 6: Exercise 1
Name:           Tim Nederveen
Time spent:		2h

Remarks:        
Sources:        
---------------}

module Exc1 where

import Data.List
import Lecture6

{-
Exercise 1

Implement a function

 exFastM :: Integer -> Integer -> Integer -> Integer
that does modular exponentiation of xyxy in polynomial time, by repeatedly squaring modulo NN.

E.g., x33mod5x33mod5 can be computed by means of

x33(mod5)=x32(mod5)×x(mod5).
x33(mod5)=x32(mod5)×x(mod5).

x32(modN)x32(modN) is computed in five steps by means of repeatedly squaring modulo NN:

x(modN)→x2(modN)→x4(modN)→…→x32(modN).
x(modN)→x2(modN)→x4(modN)→…→x32(modN).

If this explanation is too concise, look up relevant literature.
-}


exFastM :: Integer -> Integer -> Integer -> Integer
exFastM b 1 m = rem b m
exFastM b e m
    | odd e  = (rem ((exFastM b (div (e-1) 2) m) ^ 2 * b) m)
    | even e = (rem ((exFastM b (div e 2)  m) ^ 2) m)