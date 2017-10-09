import Lecture6
import Data.List
import Data.Char
import Control.Monad (when)


{--

Finding : carmichael numbers are SLOW AS FUCK when inserted in primeMR
--}
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1),
      prime (18*k+1) ]


-- primeTestsF relies on a random variable and takes the average of multiple 
-- tests, so the more test are executed (the higher nt (number of tests)) the
-- more accurate the result is
testPtf nt x y = do
    let car = carmichael !! (fromIntegral y)
    semiPrime <- primeMR nt car
    if not(semiPrime == prime car)
    then do print ("Failed " ++ show x)
            testPtf nt (x+1) y
    else do print ("Correct semi " ++ show x)
            testPtf nt (x+1) y

 
