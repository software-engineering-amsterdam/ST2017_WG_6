import Lecture6
import Data.List
import Data.Char
import Control.Monad (when)


-- primeTestsF relies on a random variable and takes the average of multiple 
-- tests, so the more test are executed (the higher nt (number of tests)) the
-- more accurate the result is
testPtf nt x y = do
    semiPrime <- primeTestsF nt x
    when (not(semiPrime == prime x)) (print ("Failed " ++ show x))
    when (x /= y) (testPtf nt (x+1) y)
