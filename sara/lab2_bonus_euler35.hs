module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lab2_4
import Lecture2
import Lab1

{-
Assignment:		Lab 2: Bonus Project Euler problem
Name:           Sara Oonk
Time spent:     1h 20m

Remarks:        Answer: There are 129 circular primes below one million.
                (read(shift(show x))::Integer - This turns an Integer into a string (show x) which is a list of Char's
                so that this list can be shifted. The result is then read back to Integer format.

Sources:        - https://projecteuler.net/problem=35
                - Lab 2 exercise 4
                - Lab 1 prime functions

---------------
Circular primes
Problem 35

The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

circularPrimesMillion :: Int
circularPrimesMillion = length ([x | x <- (primesMinMax 0 1000000 ), circularPrime x])

circularPrime :: Integer -> Bool
circularPrime x = all prime (allRotations x)

allRotations :: Integer -> [Integer]
allRotations x = findAllRotations (read(shift(show x))::Integer ) (length(show x) -1) [ read(shift(show x))::Integer]

findAllRotations :: Integer -> Int -> [Integer] -> [Integer]
findAllRotations x 0 rs = rs
findAllRotations x n rs = findAllRotations (read(shift(show x))::Integer) (n-1) (rs++[(read(shift(show x))::Integer)])
