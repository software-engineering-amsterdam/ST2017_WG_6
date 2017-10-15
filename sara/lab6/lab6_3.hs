module Lab6_3
where
import Control.Exception
import System.TimeIt

{-
    Assignment:		Lab 6: Exercise 3
    Name:           Sara Oonk
    Time spent:     1h
    Sources:        Primes from both Lecture 6 and 1.

    Comments:       At this point I figured out that measuring execution time using getCurrentTime is, surprisingly,
                    too inaccurate. It turns out the second run of anything is always relatively faster.
                    I had two options: Either construct a test in a way so that it runs a good number of times and
                    averages the running time, per method to test, OR resort to a library after all.

                    On one hand I like the idea of people being able to easily run my code, on the other hand
                    I'd like a little less experimenting on a single exercise than the previous two labs.
                    So maybe I'll take the experimental road later, and compare its results to the use of a library.
                    For now I'll deliver it with test results, using TimeIt.

                    RESULT: Prime methods from Lecture 1 are faster.

                    *Lab6_3> main
                    Comparing prime methods Lecture 1 vs Lecture 6
                    Using lecture 1:
                    55648
                    CPU time:   0.37s
                    Using lecture 6:
                    55648
                    CPU time:   0.57s

ASSIGNMENT:
Write a function composites :: [Integer] that generates the infinite list of composite natural numbers.

-}
-- Using lecture 6
composites :: [Integer]
composites = 4 : filter (not . prime) [6..]

-- Using lecture 1
composites' :: [Integer]
composites' = 4 : filter (not . primeLec1) [6..]

main = do
 putStrLn "Comparing prime methods Lecture 1 vs Lecture 6"
 putStrLn "Using lecture 1: "
 timeIt $ putStrLn (show (composites' !! 50000))
 putStrLn "Using lecture 6: "
 timeIt $ putStrLn (show (composites !! 50000))






-- Lecture 6 -----------------------------------------
primes :: [Integer]
primes = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n = factors n == [n]

factors :: Integer -> [Integer]
factors n0 = let
   ps0 = takeWhile (\ m -> m^2 <= n0) primes
 in factors' n0 ps0 where
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps)
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps


-- Lecture 1 -------------------------------------------
primesLec1 :: [Integer]
primesLec1 = 2 : filter primeLec1 [3..]

primeLec1 :: Integer -> Bool
primeLec1 n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primesLec1





