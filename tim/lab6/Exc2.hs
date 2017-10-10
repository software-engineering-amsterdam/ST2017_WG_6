{-
Assignment:		Lab 6: Exercise 2
Name:           Tim Nederveen
Time spent:		2h

Remarks:        
Sources:        
---------------}

module Exc2 where

import Data.List
import Lecture6
import Exc1
import Data.Time
import Test.QuickCheck
-- import TimeIt

{-

Exercise 2

Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results.

-}

{-
First, we perform a quick test to see if both functions return the same results using quickCheck by calling test1.

Next, we set GHCI to output execution time for each function called, using
:set +s


-}

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

test1 = quickCheckResult (\x y z -> (x >= 0 && y >= 0 && z > 0) --> exFastM x y z == expM x y z)


-- 2444442 12313239 23
    

-- runXTests :: Integer -> Integer -> Integer -> Integer
runXTests 0 _ _ _ = return ()
runXTests n base power mod = do
    start <- getCurrentTime
    print $ expM base power mod
    mid <- getCurrentTime
    print $ exFastM base power mod
    end <- getCurrentTime
    putStrLn $ "Result of (" ++ show base ++ "^" ++ show power ++ ") mod " ++ show mod ++ ":"
    putStrLn $ "expM: " ++ show (diffUTCTime mid start) ++ "\nexFastM: " ++ show (diffUTCTime end mid)
    runXTests (n-1) ((base*3)+3) ((power*3)+3) ((mod*3)+3)

main = runXTests 30 413 332 3