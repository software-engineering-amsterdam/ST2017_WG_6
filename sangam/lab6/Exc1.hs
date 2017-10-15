{--
    Assignment:     Lab 6: Assignment 1
    Name:           Sangam Gupta
    Time spent:     2 hours
--}

module Exc1 where
    
import Data.List
import Lecture6
import Data.Bits

exM' b e m  | even e = x
            | otherwise = rem (x * (b `rem` m)) m
    where x = exMM b (shiftR e 1) m

exMM _ 0 _ = 1
exMM b e m | testBit e 0  = rem (x * n) m
           | otherwise = n
        where x = rem (b^2) m
              n = exMM x (shiftR e 1) m