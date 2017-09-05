{----------------------------------------------------------------------------------------------------------------
    First I spent about 90 minutes preparing:
    - Watched the Khan Academy video on induction
    - Attempted to understand the subject better
    - Repeated the lecture sheet
    - Checking the assignment requirements and reading the first exercise
    - Setting up QuickCheck
    - Thinking about an approach, wondering what is expected

    What I think is expected here, is to translate the formulas/statements of exercise 2 and 3 to Haskell,
    in a way so that QuickCheck will take care of filling in n-values to prove the statements.

    Spent another half hour trying to figure out induction, found out I'm missing some basic maths skills,
    which I will work on at home later as I don't directly need it for the lab assignment.
-----------------------------------------------------------------------------------------------------------------}

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


{----------------------------------------------------------------------------------------------------------------
1.) Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.
    See the end of Lecture 1 for how this can be done.

    Time spent: 1h 30m
    Got the structure down, then struggled with proper notation, finally consulted teammates to get it working
-----------------------------------------------------------------------------------------------------------------}
exercise2a :: Int -> Int
exercise2a n = sum(squares n)

squares :: Int -> [Int]
squares n = map (^2) [1..n]

exercise2b :: Int -> Int
exercise2b n = (n * (n + 1) * (2 * n + 1)) `div` 6

-- QuickCheck test
testExercise2 n = (\n -> n >= 0 --> exercise2a n == exercise2b n)


exercise3a :: Int -> Int
exercise3a n = sum(cubes n)

cubes :: Int -> [Int]
cubes n = map (^3) [1..n]

exercise3b :: Int -> Int
exercise3b n = (n * (n + 1) `div` 2) ^2

-- QuickCheck test
testExercise3 n = (\n -> n >= 0 --> exercise3a n == exercise3b n)



