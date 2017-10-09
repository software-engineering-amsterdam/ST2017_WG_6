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

testPrime :: t -> [Integer] -> (t -> Integer -> IO Bool) -> IO ()
testPrime _ [] _= print "Finished"
testPrime nt (x:xs) f = do
                        semiPrime <- f nt x
                        when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
                        testPrime nt xs f

--      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- BE AWARE testEx5 and testEx6 EXTREMELY SLOW start with nt = 1 n = 1
--      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
testEx4, testEx5, testEx6 :: Int -> Int -> IO ()
testEx4 nt n = testPrime nt (take n composites) primeTestsF
testEx5 nt n = testPrime nt (take n carmichael) primeTestsF
testEx6 nt n = testPrime nt (take n carmichael) primeMR