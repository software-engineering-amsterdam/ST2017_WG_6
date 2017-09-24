{-
Assignment:		Lab 3: Assignment 4
Name:           Sangam Gupta
Time spent:     5 hours
---------------}
module Exc4 where

import Data.List
import System.Random
import Lecture3
import Exc3
import Exc1

{-------------------------------------------------------------------------------------------------------------------------------------
4)  Write a formula generator for random testing of properties of propositional logic,
    or teach yourself enough QuickCheck to use random QuickCheck testing of formulas.

    Use your random testing method to test the correctness of the conversion program
    from the previous exercise. Formulate a number of relevant properties to test,
    and carry out the tests, either with your own random formula
    generator or with QuickCheck.

    Deliverables: generator for formulas, sequence of test properties,
    test report, indication of time spent.
--------------------------------------------------------------------------------------------------------------------------------------}

-- Random function copied from Lecture 2 and limited to 4 because there
-- are four cases.
getRandomInt :: IO Int
getRandomInt = getStdRandom (randomR (1,5))


{-- This random form generator does not create a complete random form
    but is limited by depht d. For example if you take d = 2 then the max
    number of connectives is 2 trees of depht 2 with each max two childeren.

    Besides that we limited Cnj and Dsj to max two forms. So the generation
    of forms doesn't take too long and it also keeps the (extra) code to a minimum.
    If needed you could also randomly generate the number of forms in Cnj and Dsj
    but this will suffice for testing purposes.
 --}
randomForm :: Int -> IO Form
randomForm 0 = Prop <$> getRandomInt
randomForm d = do q <- getRandomInt
                  k <- randomForm (d-1)
                  m <- randomForm (d-1)
                  case q of
                    1 -> return $ Neg k
                    2 -> return $ Equiv k m
                    3 -> return $ Impl k m
                    4 -> return $ Cnj [k,m] -- Limited to two
                    5 -> return $ Dsj [k, m] -- Limited to two

{-- To test if convertToCNF, we check if the
    original form and the converted form are logical equivelant.

    Checking for only logical equivalence isn't enough to test if the
    function really does convert to CNF form. It could for example just
    return the original form which would succeed in this test.

    So we added a second test to check if the output of convertToCNF has a
    valid CNF format.

    We run both test together against 100 randomly generated forms.

    The result is that all 100 succeed. so convertToCNF is correctly implemented
--}

testm :: (Form -> Bool) -> Int -> Int -> IO ()
testm func k n 
    | k == n = print (show n ++ " tests passed")
    | otherwise = do 
                  x <- randomForm 3
                  if func x
                     then do testm func (k+1) n
                  else error ("failed test on: " ++ show x)

testExc4 :: Form -> Bool
testExc4 x = equiv x (convertToCNF x) && checkCNF (convertToCNF x)

checkCNF :: Form ->  Bool
checkCNF f
    | arrowfree f /= f = False
    | nnf f /= f = False
    | otherwise = isCNF f
    where
        isCNF :: Form -> Bool
        isCNF (Cnj f) = all isCNF f
        isCNF (Dsj (f1:ft)) = dsjCheck f1 && isCNF (Dsj ft)
        isCNF f = True

        dsjCheck :: Form -> Bool
        dsjCheck (Cnj fs) = False
        dsjCheck (Dsj (f1:ft)) = dsjCheck f1 && dsjCheck (Dsj ft)
        dsjCheck x = True

main = testm testExc4 1 100