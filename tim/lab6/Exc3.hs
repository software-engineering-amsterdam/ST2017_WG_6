{-
Assignment:		Lab 6: Exercise 3
Name:           Tim Nederveen
Time spent:		1h

Remarks:        
Sources:        
---------------}

module Exc3 where

import Data.List
import Lecture6

{-
Exercise 3

In order to test Fermat's Primality Check (as implemented in function prime_test_F), the list of prime numbers generated 
by Eratosthenes' sieve is useless, for Fermat's Primality Check correctly classify the primes as primes. Where the check 
can go wrong is on classifying composite numbers; these can slip through the Fermat test.

Write a function composites :: [Integer] that generates the infinite list of composite natural numbers.
-}

-- primeTestF :: Integer -> IO Bool
-- primeTestF n = do 
--    a <- randomRIO (2, n-1) :: IO Integer
--    return (exM a (n-1) n == 1)

composites :: [Integer]
composites = filter (not.prime) [2..]