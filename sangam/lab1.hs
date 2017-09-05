module Lab1 where
import Data.List
import Test.QuickCheck


--Excersise 1
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


-- Excersise 2
-- Used subsequences to generate a powerSet.
-- Currently this solution works until you hit test 44 on my machine.
-- 1 hour
testExc2 = quickCheck (\n -> powerSetProof n)

powerSetProof :: [Int] -> Bool
powerSetProof xs = length (subsequences xs) == (2^c) where c = length xs
