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


-- primeTestsF relies on a random variable and takes the average of multiple 
-- tests, so the more test are executed (the higher nt (number of tests)) the
-- more accurate the result is
testPtf nt x y = do
    let car = carmichael !! (fromIntegral y)
    semiPrime <- primeTestsF nt car
    when (not(semiPrime == prime car)) (print ("Failed " ++ show x))
    when (x /= y) (testPtf nt (x+1) y)

-- Even when compiling testing 1 carmichael number takes more than 15 seconds
main = testPtf 1 1 1
