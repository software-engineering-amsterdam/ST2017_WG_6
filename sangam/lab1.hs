module Lab1 where
import Data.List
import Test.QuickCheck


-- ********* Excercise 1 *********
-- 2 hours including setup

-- Use implication operator
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Create factorial function
factorial :: Int -> Int
factorial 0 = 0
factorial n = (n^2) + factorial (n - 1)

proofFactorial :: Int -> Int
proofFactorial 0 = 0
proofFactorial n = (n * (n + 1) * (2 * n + 1)) `div` 6

-- Not correct
-- testExc1 :: Int -> Bool
-- testExc1 n | n < 0 = True
--            | otherwise = factorial n == proofFactorial n

-- B variant
testExc1a = quickCheck (\n ->  n >= 0 --> factorial n == proofFactorial n)

factorialPower3 :: Int -> Int
factorialPower3 0 = 0
factorialPower3 n = (n^3) + factorialPower3 (n - 1)

proofFactorial2 :: Int -> Int
proofFactorial2 0 = 0
proofFactorial2 n = ((n * (n+1)) `div` 2) ^ 2

testExc1b = quickCheck (\n ->  n >= 0 --> factorialPower3 n == proofFactorial2 n)


-- ********* Excercise 2 *********
-- Used subsequences to generate a powerSet.
-- TODO Antwoord toevoegen op de vraag
-- 1 hour
testExc2 = quickCheck (\n -> powerSetProof n)

powerSetProof :: [Int] -> Bool
powerSetProof xs = length (subsequences xs) == (2^c) where c = length xs


-- ********* Excercise 3 *********
-- TODO Antwoord toevoegen op de vraag
-- permutations is n * n-1 * .. * (n-i) > 0 where n is length of an array
-- Or in other words it is the same as n * n + 1 * .. * (n+i) < length of array is reached and n > 0
-- 1 hour
perms :: [Int] ->[[Int]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
 insrt x [] = [[x]]
 insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

permsLength :: Int -> Int
permsLength 0 = 1
permsLength n = n * permsLength (n-1)

testExc3 = quickCheck (\n -> length (perms n) == permsLength (length n))

-- ********* Excersise 4 *********
-- 1 hour
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Check if the function reversal reverses any prime number.
-- For non prime numbers like -10 and 10000 the reversal will not work since the reversal of 10000 is 00001 and -10 is 10-.
-- Therefore we only check if the reversal to all the prime numbers in 0..10000
reversal :: Integer -> Integer
reversal = read . reverse . show

testExc4a :: Integer -> Bool
testExc4a x = x == (reversal (reversal x))

-- Get all prime pairs and check if the pairs are reversals of each other

-- First solution I came up with
-- primePairs :: [Integer] -> [Integer]
-- primePairs xs = [x | x <- xs, (prime x) && (prime (reversal x))]

primeReversalPairs :: [Integer]
primeReversalPairs = takeWhile( < 10000) (filter (prime . reversal) primes)

-- ********* Excercise 5 *********
-- Check if the sum of 101 consecutive primes result in prime
-- primes is an infinite array, we first check if index 0 till 100 so we take the first 101 of the infinite list.
-- If the sum is not a prime we increment index to 1 till 101 and check again if the sum is a prime. etc.
consPrimes :: [Integer] -> [Integer]
consPrimes xs = take 101 xs

sumPrimesIsPrime :: [Integer] -> Bool
sumPrimesIsPrime xs =  prime(sum(xs))

-- Check subset primes for prime. If the sum is a prime then return the sum else redo with tail (so exluding the first element)
recusivePrimeSumCheck xs = if sumPrimesIsPrime (consPrimes(xs)) then sum(consPrimes(xs)) else recusivePrimeSumCheck (tail xs)

-- TODO how to test? answer questions
testExc5 = recusivePrimeSumCheck primes


-- ********* Excercise 6 *********
-- We check starting from 2 if the product is a prime and keep adding the nextprime until the we find a counterexample
-- TODO add answers to questions
-- 1 hour
counterExampleCheck :: [Integer] -> Bool
counterExampleCheck xs = prime ((product xs) + 1)

-- Take takes Int
takePrimes :: Int -> [Integer]
takePrimes x = take x primes

recursiveCounterExample :: Int -> [Integer]
recursiveCounterExample x = if (not (counterExampleCheck (takePrimes x))) then takePrimes x else recursiveCounterExample (x+1)

-- x is one since we want to start with the check at index 1
testExc6 = recursiveCounterExample 1
