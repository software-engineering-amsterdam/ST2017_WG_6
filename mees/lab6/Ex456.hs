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


testPrime _ [] _= print "Finished"
testPrime nt (x:xs) f = do
                        semiPrime <- f nt x
                        when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
                        testPrime nt xs f

testEx4 = testPrime 1 (take 100 composites) primeTestsF

testEx5 = testPrime 1 (take 2 carmichael) primeTestsF

testEx6 = testPrime 1 (take 2 carmichael) primeMR