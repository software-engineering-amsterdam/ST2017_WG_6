{--
    Assignment:     Lab 6: Assignment 1
    Name:           Sangam Gupta
    Time spent:     2 hours
--}

module Exc1 where

import Data.List
import Lecture6
import Data.Bits

-- expMM :: Integer -> Integer -> Integer -> Integer
-- expMM x e y = rem (exM' x e y 0) y

-- exM' :: Integer -> Integer -> Integer -> Int -> Integer
-- exM' x e y p | shiftR e p == 0  = 1
--              | testBit e p      =  n * (x ^ bit' p `rem` y) 
--              | otherwise        = n
--         where n = exM' x e y (p+1)


-- bit' :: Int -> Int
-- bit' = bit