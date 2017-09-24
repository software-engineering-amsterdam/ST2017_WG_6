-- To prevent circular dependecies we conducted some extra tests for  certain exercises
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Exc1
import Exc3
import Exc4

-- Lab 3 Exercise 1
-- Since only manual teste are quite error prone, you yourself specified the input
-- of which you know it will be right. Therefore we also conducted some extra
-- automated tests on the functions for exercise 1 with our own quicheck implementation
-- and the random Form generator created in exercise 4.

quickCont, quickTau :: Form -> Bool
quickCont f = (contradiction f) == all (\ v -> not(evl v f)) (allVals f)
quickTau f = tautology f == all (\ v -> evl v f) (allVals f)

quickEnt :: Form -> Form -> Bool
quickEnt f1 f2 = entails f1 f2 == tautology (Impl f1 f2)
quickEq f1 f2 = equiv f1 f2 == tautology (Equiv f1 f2)


quickCheckOneForm :: (Form -> Bool) -> Int -> Int -> IO ()
quickCheckOneForm func 0 depth = print ("++ All tests passed")
quickCheckOneForm func k depth = do
                x <- randomForm depth
                if func x
                then quickCheckOneForm func (k - 1) depth
                else error ("failed test on: " ++ show x)

quickCheckTwoForms :: (Form -> Form -> Bool) -> Int -> Int -> IO ()
quickCheckTwoForms func 0 depth = print ("++ All tests passed")
quickCheckTwoForms func k depth = do
                x <- randomForm depth
                y <- randomForm depth
                if func x y
                then quickCheckTwoForms func (k - 1) depth
                else error ("failed test on: " ++ show x ++ " AND " ++ show y)

main = do
    quickCheckOneForm quickCont numTests depth
    quickCheckOneForm quickTau numTests depth
    quickCheckTwoForms quickEnt numTests depth
    quickCheckTwoForms quickEq numTests depth
    where numTests = 100
          depth = 4