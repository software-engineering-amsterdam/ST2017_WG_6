import Lecture6
import Data.List
import Data.Char
import Data.Bits

-- Straight forward implementation of modlar exponentiation
exMstraight :: Integer -> Integer -> Integer -> Integer
exMstraight x y n = x^y `mod` n 

-- Memory efficiancy implementation of modular exponentation as 
-- described on https://en.wikipedia.org/wiki/Modular_exponentiation
exMmemeff :: Integer -> Integer -> Integer -> Integer
exMmemeff _ 0 _ = 1
exMmemeff x y n = (x * (exMmemeff x (y -1) n)) `mod` n

-- Algorithm from assignment implemented
exMass :: Integer -> Integer -> Integer -> Integer
exMass _ 0 _ = 1
exMass x y n | even y = exMass (mod (x^2) n ) (div y 2) n
             | otherwise = (mod (x * (exMass x (y -1) n)) n)

-- Binary thing of it
exMf :: Integer -> Integer -> Integer -> Integer
exMf _ 0 _ = 1
exMf x y n = let z | testBit y 0 = mod x n 
                   | otherwise = 1 
             in mod (z * (exMf (mod (x^2) n) (shiftR y 1) n)) n
