{-
Assignment:		Lab 1: Functional Programming and (Some) Logic
Name:           Tim Nederveen
UVA id:      	11959037
Remarks:        --
Sources:        --
-}

module Lab1Tim where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


-- question 1
-- todo: now works for negative numbers
funcOneLeft :: Integer -> Integer
funcOneLeft n
    | n <= 1    = n
    | otherwise = n^2 * funcOneLeft (n-1)

funcOneRight :: Integer -> Integer
funcOneRight n = n * (n+1) * (2*n + 1) `div` 6

-- funcOne :: Integer -> Bool
-- funcOne n = funcOneLeft n == funcOneRight n

answer1 = quickCheckResult (\n -> n >= 0 --> (funcOneLeft n == funcOneRight n))


-- question 2
funcTwoLeft :: [a] -> Integer
funcTwoLeft n = 2^(length n)

funcTwoRight :: [a] -> Integer
funcTwoRight n = toInteger (length (subsequences n))

-- answer2 = quickCheckResult (\n -> (n >= 0) --> (funcTwoLeft n == funcTwoRight n))


-- question 3
permsLength :: [a] -> Integer
permsLength n = toInteger (length (perms n))

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

answer3 = quickCheckResult (\n -> (n >= 0) --> (permsLength [1..n] == product [1..n]))


-- question 4
reversal :: Integer -> Integer
reversal = read . reverse . show

reversedPrime :: Integer -> Bool
reversedPrime n  = prime (reversal n)

answer4 = takeWhile (<10000) (filter reversedPrime primes)


-- question 5
sublistPrimes :: [Integer] -> Bool
sublistPrimes n = prime (sum n)

recursiveReturnSublist :: Int -> [a] -> [[a]]
recursiveReturnSublist n l = (take n l) : recursiveReturnSublist n (tail l)

answer5 = sum (head (filter sublistPrimes (recursiveReturnSublist 101 primes)))


-- question 6
recursiveReturnSublist2 :: Int -> [a] -> [[a]]
recursiveReturnSublist2 n l = (take n l) : recursiveReturnSublist2 (n+1) l

conjecture :: [Integer] -> Bool
conjecture xs = prime ((product xs) + 1)

answer6 = head (filter (not.conjecture) (recursiveReturnSublist2 1 primes))


-- question 7
luhn :: Integer -> Bool
luhn n = (sum (replaceDigits (intToList n)) `mod` 10) == 0

replaceDigits :: [Integer] -> [Integer]
replaceDigits [] = []
replaceDigits [x] = [x]
replaceDigits (x:y:zs) = x : modDigit y : replaceDigits zs

modDigit :: Integer -> Integer
modDigit y
    | y < 5     = 2*y
    | otherwise = (2*y) - 9

intToList :: Integer -> [Integer]
intToList n
    | n < 10    = [n]
    | otherwise = [n `mod` 10] ++ intToList (n `div` 10)

-- last function is in O(n)
isAmericanExpress :: Integer -> Bool -- 15 numbers
isAmericanExpress n = (last (intToList n) == 3) && (length (intToList n) == 15) && luhn n

isMaster :: Integer -> Bool -- 16 numbers
isMaster n = (last (intToList n) == 5) && (length (intToList n) == 16) && luhn n

isVisa :: Integer -> Bool -- 13 or 16 numbers
isVisa n = (last (intToList n) == 4) && (length (intToList n) == 13 || length (intToList n) == 16) && luhn n


-- question 8
data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- accuses :: Boy -> Boy -> Bool
-- accuses Matthew b = not (b == Matthew) && not (b == Carl)
-- accuses Peter b   = b == Jack || b == Matthew
-- accuses Jack b    = not (accuses Matthew b) && not (accuses Peter b)
-- accuses Arnold b  = accuses Peter b /= accuses Matthew b
-- accuses Carl b    = not (accuses Arnold b)

-- accusers :: Boy -> [Boy]
-- accusers b = map 

-- guilty :: [Boy]


-- honest :: [Boy]

