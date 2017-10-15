{--
    Assignment:     Lab 6: Assignment 3
    Name:           Sangam Gupta
    Time spent:     30 minuts
--}
module Exc3 where
    
import Data.List
import Lecture6

composites' :: [Integer]
composites' = [x | x <-[1..], not (prime x)]