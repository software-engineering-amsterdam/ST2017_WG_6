import Lecture6
import Data.List
import Data.Char
import Control.Monad (when)


carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1),
      prime (18*k+1) ]

testPtf nt x y = do
    semiPrime <- primeTestsF nt x
    when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
    when (x /= y) (testPtf nt (x+1) y)

testPtf nt x y = do
    let car = carmichael !! (fromIntegral y)
    semiPrime <- primeTestsF nt car
    when (not(semiPrime == prime car)) (print ("Failed " ++ show x))
    when (x /= y) (testPtf nt (x+1) y)

testPtf nt x y = do
    let car = carmichael !! (fromIntegral y)
    semiPrime <- primeMR nt car
    when (not(semiPrime == prime car)) (print ("Failed " ++ show x))
    when (x /= y) (testPtf nt (x+1) y)

testPrime _ [] _= print "Finished"
testPrime nt (x:xs) f = do
                        semiPrime <- f nt x
                        when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
                        testPrime nt xs f

test = testPrime 1 (take 100 composites) primeTestsF