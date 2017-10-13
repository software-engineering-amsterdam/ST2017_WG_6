{-
Ex1 - 1 hours
Ex2 - ????????????????????  hours TODO
Ex3 - 0.01 hours
Ex4 - 1,5 hours
Ex5 - 0.5 hours
Ex6 - 0.6 hours
Ex6 - 1,5 hours
-}

module Exercises where
import Lecture6
import Control.Monad (when)
import Data.Bits

-- Ex1 - Binary modular exponentiation
-- We implemented the fast modular exponentiation as described in the assignments
-- and combined it with the wikipedia bitshifting approach to aquire a great
-- speed-up. (this function is also included in Lecture6.hs)
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x y n = let z | testBit y 0 = mod x n 
                   | otherwise = 1 
             in mod (z * (exM' (mod (x^2) n) (shiftR y 1) n)) n

-- Ex3 - Infinite composites list
-- Primes are only devisable by 1 and itself and resulting in another natural 
-- number. Composites are a positive integer that has at least one divisor other
-- than 1 and itself, meaning all positive numbers which are not primes
-- are composites.
composites' :: [Integer]
composites' = filter (not.prime) [2..]

-- Test function for excersize 4, 5 and 6
-- Given a numner of test 'nt', a list of numbers we want to test 'x:xs' and a
-- test function 'f' this function will calculate if the test function can
-- accuratly calculates if a number is a prime, if it is not correct we print 
-- it, otherwise we continue with the next number in the list
testPrime :: t -> [Integer] -> (t -> Integer -> IO Bool) -> [Char] -> IO ()
testPrime _ [] _ msg      = putStr ("Finished " ++ msg)
testPrime nt (x:xs) f msg = do
                        semiPrime <- f nt x
                        when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
                        testPrime nt xs f msg

-- Ex6 find mersene primes
-- Calculate mersene primes  bases on the Miller Rabin algorithm, give a number
-- of tests 'nt' and from which primes to start 'y' and on which prime to stop 
-- 'n', where n-y is the number of primes searched.
findMersene :: Int -> Int -> Int -> IO ()
findMersene nt y n = do
            let semiPrime = (2 ^ (primes' !! y) - 1 )
            mersenne <- primeMR nt semiPrime
            when (mersenne) (print (show (primes' !! y) ++ " Marsenne found " ++ show semiPrime))
            when (y /= n) (findMersene nt (y + 1) n)

-- Additonal:
-- 2x faster implemenation of primes function, but the primes function is not
-- the bottleneck so no measurable speed-up for calculdating mersene primes.
-- Source: https://www.reddit.com/r/haskell/comments/35vc31/the_real_way_to_generate_a_list_of_primes_in/
primes' = 2 : 3 : 5 : primes''
  where
    isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
    primes'' = 7 : filter (isPrime primes'') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

