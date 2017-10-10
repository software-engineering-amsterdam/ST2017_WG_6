{--
    Assignment:     Lab 6: Assignment 5
    Name:           Sangam Gupta
    Time spent:     1 hour
--}
module Exc5 where
    
import Data.List
import Exc4
import Lecture6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       prime (6*k+1), 
       prime (12*k+1), 
       prime (18*k+1) ]

main5 = do c <- test carmichael 1
           print c
           c1 <- test carmichael 2
           print c1
        --    c2 <- test carmichael 3
        --    print c2