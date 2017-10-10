import Lecture6
import Data.List
import Data.Char
import Control.Monad (when)
import Data.Bits


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1),
      prime (18*k+1) ]

-- Ex1 - Binary modular exponentiation
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x y n = let z | testBit y 0 = mod x n 
                   | otherwise = 1 
             in mod (z * (exM' (mod (x^2) n) (shiftR y 1) n)) n

-- Ex3 - Infinite composites list
composites' :: [Integer]
composites' = filter (not.prime) [2..]

-- Test function for excersize 4,5 and 6
testPrime :: t -> [Integer] -> (t -> Integer -> IO Bool) -> IO ()
testPrime _ [] _= print "Finished"
testPrime nt (x:xs) f = do
                        semiPrime <- f nt x
                        when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
                        testPrime nt xs f

-- Execution of test of exercise 4,5 and 6
testEx4, testEx5, testEx6 :: Int -> Int -> IO ()
testEx4 nt n = testPrime nt (take n composites) primeTestsF
testEx5 nt n = testPrime nt (take n carmichael) primeTestsF
testEx6 nt n = testPrime nt (take n carmichael) primeMR

-- Ex6 find mersene primes
findMersene :: Int -> Int -> Int -> IO ()
findMersene nt y n = do
            let semiPrime = (2 ^ (primes !! y) - 1 )
            mersenne <- primeMR nt semiPrime
            when (mersenne) (print ("Marsenne found " ++ show semiPrime))
            when (y /= n) (findMersene nt (y + 1) n)
