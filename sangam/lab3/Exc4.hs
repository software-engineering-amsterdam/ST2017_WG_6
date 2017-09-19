module Exc4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{-------------------------------------------------------------------------------------------------------------------------------------
4)  Write a formula generator for random testing of properties of propositional logic,
    or teach yourself enough QuickCheck to use random QuickCheck testing of formulas.

    Use your random testing method to test the correctness of the conversion program
    from the previous exercise. Formulate a number of relevant properties to test,
    and carry out the tests, either with your own random formula
    generator or with QuickCheck.

    Deliverables: generator for formulas, sequence of test properties,
    test report, indication of time spent.

    Time spent: 
--------------------------------------------------------------------------------------------------------------------------------------}

getRandomInt :: IO Int
getRandomInt = getStdRandom (randomR (0,4))

randomForm 0 = do Prop <$> getRandomInt
randomForm d = do q <- getRandomInt
                  case q of
                    0 -> do
                        Neg <$> randomForm (d-1)
                    1 -> do
                        k <- randomForm (d-1)
                        m <- randomForm (d-1)
                        return $ Equiv (k) (m)
                    2 -> do
                        k <- randomForm (d-1)
                        m <- randomForm (d-1)
                        return $ Impl (k) (m)
                    3 -> do
                        k <- randomForm (d-1)
                        m <- randomForm (d-1)
                        return $ Cnj[(k), (m)] -- Limited to two
                    4 -> do
                        k <- randomForm (d-1)
                        m <- randomForm (d-1)
                        return $ Dsj[(k), (m)] -- Limited to two
