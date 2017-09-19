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

-- Random function copied from Lecture 2 and limited to 4 because there
-- are four cases.
getRandomInt :: IO Int
getRandomInt = getStdRandom (randomR (0,4))


{-- This random form generator does not create a complete random form
    but is limited by depht d. For example if you take d = 2 then the max
    number of connectives is 2 trees of depht 2 with each max two childeren.

    Besides that we limited Cnj and Dsj to max two forms. So the generation
    of forms doesn't take too long and it also keeps the (extra) code to a minimum.
    If needed you could also randomly generate the number of forms in Cnj and Dsj
    but this will suffice for testing purpose.
 --}
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

{-- To test if convertToCNF, we check if the
    original form and the converted form are logical equivelant.

    Checking for only logical equivalence isn't enough to test if the
    function really does convert to CNF form. It could for example just
    return the original form which would succeed in this test.

    So we added a second test to check if the output of convertToCNF has a
    valid CNF format.

    We run both test together against 100 randomly generated forms.

    The result is that all 100 succeed. so convertToCNF is correctly implemended
--}
test100 = test 1 100

test :: Int -> Int -> IO ()
test k n = if k == n then print (show n ++ " tests passed")
                else do
                  x <- randomForm 3 -- This can be changed to a random int. To increase the randomness of the test b
                  if (equiv x (convertToCNF x)) && checkCNF (convertToCNF x) then
                    do print ("pass on: " ++ show x)
                       test (k+1) n
                  else error ("failed test on: " ++ show x)


--TODO remove code from here and reuse modulez
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Logical equivalence
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)

checkCNF :: Form ->  Bool
checkCNF f
    | arrowfree f /= f = False
    | nnf f /= f = False
    | otherwise = isCNF f
    where
        isCNF :: Form -> Bool
        isCNF (Cnj f) = all isCNF f
        isCNF (Dsj (f1:ft)) = (dsjCheck f1) && (isCNF (Dsj ft))
        isCNF f = True

        dsjCheck :: Form -> Bool
        dsjCheck (Cnj fs) = False
        dsjCheck (Dsj (f1:ft)) = (dsjCheck f1) && (dsjCheck (Dsj ft))
        dsjCheck x = True


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
