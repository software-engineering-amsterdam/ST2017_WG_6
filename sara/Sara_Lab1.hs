{-
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
-}

module Lab1 where
import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

exercise2a :: Int -> Int
exercise2a n = sum(squares n)

exercise2b :: Int -> Int
exercise2b n = (n * (n + 1) * (2 * n + 1)) `div` 6


testExercise2 n = (\n -> n >= 0 --> exercise2a n == exercise2b n)


squares n = map (^2) [1..n]

