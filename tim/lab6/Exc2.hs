{-
Assignment:		Lab 6: Exercise 2
Name:           Tim Nederveen
Time spent:

Remarks:        
Sources:        
---------------}

module Exc2 where

import Data.List
import Lecture6
import Exc1
import Data.Time
-- import TimeIt

{-

Exercise 2

Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results.

-}

{-
First, we set GHCI to output execution time for each function called, using
:set +s


-}


-- test1 = quickCheckResult (\x y z -> (x >= 0 && y >= 0 && z >= 0) --> orderIrrelevant x y z)


-- 2444442 12313239 23
    

-- runXTests :: Integer -> Integer -> Integer -> Integer
runXTests 0 _ _ _ = return ()
runXTests n base power mod = do
    start <- getCurrentTime
    let slowVar = expM base power mod
    mid <- getCurrentTime
    let fastVar = exFastM base power mod
    end <- getCurrentTime
    putStrLn $ "Result of (" ++ show base ++ "^" ++ show power ++ ") mod " ++ show mod ++ ":"
    putStrLn $ "expM: " ++ show (diffUTCTime mid start) ++ " exFastM: " ++ show (diffUTCTime end mid)
    runXTests (n-1) (base*6) (power*6) (mod*6)

main = runXTests 10 12 22 3