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

    Time spent: 5h
--------------------------------------------------------------------------------------------------------------------------------------}

getRandomInt :: IO Int
getRandomInt = getStdRandom (randomR (0,4))

randomForm :: Int -> IO Form
randomForm 0 = do Prop <$> getRandomInt
randomForm d = do q <- getRandomInt
                  k <- randomForm (d-1)
                  m <- randomForm (d-1)
                  case q of
                    0 -> do
                        return $ Neg k
                    1 -> do
                        return $ Equiv (k) (m)
                    2 -> do
                        return $ Impl (k) (m)
                    3 -> do
                        return $ Cnj [(k), (m)] -- Limited to two
                    4 -> do
                        return $ Dsj [(k), (m)] -- Limited to two

test = do
          print "Generated Form: "
          x <- randomForm 2
          print x
          print "convertToCNF Form:"
          print (convertToCNF x)
          print "Are the same?"
          print (equiv x (convertToCNF x))

test100 = testR 1 100 convertToCNF equiv


testR :: Int -> Int -> (Form -> Form) -> (Form -> Form -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  x <- randomForm 2
                  if r x (f x) then
                    do print ("pass on: " ++ show x)
                       testR (k+1) n f r
                  else error ("failed test on: " ++ show x)


--TODO remove code from here and reuse modulez
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)


convertToCNF :: Form ->  Form
convertToCNF = toCNF . nnf . arrowfree
    where
        toCNF :: Form -> Form
        toCNF (Prop x) = Prop x
        toCNF (Neg (Prop x)) = (Neg (Prop x))
        toCNF (Cnj fs) = Cnj (map toCNF fs)
        toCNF (Dsj [a]) = toCNF a
        toCNF (Dsj (f1:f2)) = disLaw (toCNF f1) (toCNF (Dsj f2))

        disLaw :: Form -> Form -> Form
        disLaw (Cnj []) _ = Cnj []
        disLaw (Cnj [f1]) f2 = disLaw f1 f2
        disLaw (Cnj (f11:f12)) f2 = Cnj [(disLaw f11 f2), (disLaw (Cnj f12) f2)]
        disLaw f1 (Cnj (f21:f22)) = Cnj [(disLaw f1 f21), (disLaw (Cnj f22) f1)]
        disLaw f1 f2 = Dsj [f1, f2]

a = Prop 1
b = Prop 2
c = Prop 3
d = Prop 4

-- Should be (after cnf): (A ∧ ((B ∨ C) ∧ (B ∨ D)))
form4 = Cnj [a, (Dsj [b, (Cnj [c,d])])]

-- Truth Table, origin vs converted
ttOriginVSConverted f = zipWith (==) (map (\ v -> evl v f) (allVals f)) (map (\ v -> evl v (convertToCNF f)) (allVals f))

r12 = parse "+(+(-1 2))"
