{-------------------------------------------------------------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------------------------------------------------------------}

module Lab1 where
import Data.List
import Test.QuickCheck


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

{-------------------------------------------------------------------------------------------------------------------------------------
1.) Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.
    See the end of Lecture 1 for how this can be done.

    Time spent: 1h 30m
    Got the structure down, then struggled with proper notation, finally consulted teammates to get it working
--------------------------------------------------------------------------------------------------------------------------------------}
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



{-------------------------------------------------------------------------------------------------------------------------------------
2.) Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
    You can use subsequences :: [a] -> [[a]] for the list of all subsequences of a given list.

    'Prove by induction that if A is a finite set with |A|=n, then |P(A)|=2n.'
--------------------------------------------------------------------------------------------------------------------------------------}
exercise4a :: Int -> Int
exercise4a n = length(subsequences[1..n])

exercise4b :: Int -> Int
exercise4b n = 2 ^n

testExercise4 n = (\n -> n > 0 && n <= 20 --> exercise4a n == exercise4b n)
{-------------------------------------------------------------------------------------------------------------------------------------
----'Is the property hard to test? If you find that it is, can you given a reason why?'

    It's pretty simple, but it turned out that it'll take 'forever' if you don't limit QuickCheck's input value.
    At a max of 20 it runs fine, at >30 it's significantly slow. The higher the number n, the more subsets to generate.


----'Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually?
----Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification?
----Or are you testing something else still?'

    You are indeed testing both a mathematical fact and the specification of subsequence by using that fact.
    You could say that you're also testing calculation speed when you don't limit QuickCheck's input values,
    but then you would actually have to measure the time it takes and compare that to some expectation for it to make sense.

    Total time spent on ex2: 30m
 --------------------------------------------------------------------------------------------------------------------------------------}




{-------------------------------------------------------------------------------------------------------------------------------------
3.) Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
----A permutation of a list is a reordering of the members of a list.
----You can also use the Data.List function permutations.
----Find a formula (closed form) for the number of permutations of a list of n distinct objects, and prove your guess by induction.

    My guess is cartesian product n X n
    I'm also guessing this test will also run slow without a limit n, although not as bad as 2.
--------------------------------------------------------------------------------------------------------------------------------------}
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

exercise5a :: Int -> Int
exercise5a n = length(perms[1..n])

--Wrong guess!
--exercise5b :: Int -> Int
--exercise5b n = n ^2

testExercise5 n = (\n -> n > 0 && n <= 10 --> exercise5a n == exercise5b n)

exercise5b :: Int -> Int
exercise5b n = factorial n--Readability

factorial :: Int -> Int
factorial 0 = 0;
factorial 1 = 1;
factorial n = n * factorial (n-1)
{-------------------------------------------------------------------------------------------------------------------------------------
    I guessed wrong, and asked teammates what the correct formula was. Implemented the solution on my own.

----'Is the property hard to test? If you find that it is, can you given a reason why?'
    It's not that hard to test but the effect turned out to be even worse than exercise 3, coming to a seemingly complete halt.
    For it to run reasonably fast I limited it to 10 n. I think it has to do with the factorial values exceeding the data limit
    for large n's. The values then build up really high really fast.

----Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually?
----Are you checking a mathematical fact? Or are you testing whether perms satisfies a part of its specification?
----Or are you testing something else still?
    Again both, and indirectly the limits of calculating factorials using Ints.

    Total time spent on ex3: 1h
--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
4.) The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime.
    Write a function that finds all primes < 10000 with this property.
--------------------------------------------------------------------------------------------------------------------------------------}
reversal :: Integer -> Integer
reversal = read . reverse . show

--Modified prime and primes from Lecture 1
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
 where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..10000]

reversiblePrimes :: [Integer]
reversiblePrimes = filter reversible primes

reversible :: Integer -> Bool
reversible n = prime (toInteger(reversal n))
{-------------------------------------------------------------------------------------------------------------------------------------
    'How would you test this function, by the way?'
    I would want to check whether every number with at least two digits that reversiblePrimes returns has a buddy.
    This could be done by checking for reversals in the list, or possibly even faster when iterating through the
    list x the list, though Haskell's lists appear to be singly-linked and queue-like (because you can easily take
    heads) so it takes a full iteration to reach the last element.
    This isn't suitable for a QuickCheck test because this isn't about testing against random values, we're using
    the same subset of the set of all primes < 10000 each time.

    I could probably attempt a bit like a unittest?
--------------------------------------------------------------------------------------------------------------------------------------}
testReversiblePrimesValidChecker :: [Integer] -> [Integer] -> Bool
testReversiblePrimesValidChecker [] ys = True
testReversiblePrimesValidChecker _ [] = False
testReversiblePrimesValidChecker (x:xs) ys = (elem (toInteger(reversal x)) ys) && testReversiblePrimesValidChecker xs ys

testReversiblePrimesValid :: [Integer] -> Bool
testReversiblePrimesValid xs = testReversiblePrimesValidChecker xs xs
{-------------------------------------------------------------------------------------------------------------------------------------
Performance could probably be better, but testing this with reversiblePrimes returns True,
testing it with (reversiblePrimes ++ [12]) returns False.

Total time spent on ex4: 1h 30m
--------------------------------------------------------------------------------------------------------------------------------------}
