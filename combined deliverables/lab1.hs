module Lab1 where
import Data.List
import Test.QuickCheck

{-------------------------------------------------------------------------------------------------------------------------------------
Helpers
--------------------------------------------------------------------------------------------------------------------------------------}

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

{-------------------------------------------------------------------------------------------------------------------------------------
1.) Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.
    See the end of Lecture 1 for how this can be done.
--------------------------------------------------------------------------------------------------------------------------------------}

exercise2a :: Int -> Int
exercise2a n = sum(squares n)

squares :: Int -> [Int]
squares n = map (^2) [1..n]

exercise2b :: Int -> Int
exercise2b n = (n * (n + 1) * (2 * n + 1)) `div` 6

-- QuickCheck test
test11 = quickCheckResult (\n -> n >= 0 --> exercise2a n == exercise2b n)


exercise3a :: Int -> Int
exercise3a n = sum(cubes n)

cubes :: Int -> [Int]
cubes n = map (^3) [1..n]

exercise3b :: Int -> Int
exercise3b n = (n * (n + 1) `div` 2) ^2

-- QuickCheck test
test12 = quickCheckResult (\n -> n >= 0 --> exercise3a n == exercise3b n)

{-------------------------------------------------------------------------------------------------------------------------------------
2.) Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
    You can use subsequences :: [a] -> [[a]] for the list of all subsequences of a given list.

    'Prove by induction that if A is a finite set with |A|=n, then |P(A)|=2n.'
--------------------------------------------------------------------------------------------------------------------------------------}
subEqn2 :: Int -> Bool
subEqn2 n = length(subsequences[1..n]) == 2 ^ n

test2 = quickCheckResult (\n -> (n >= 0 && n < 20) --> (subEqn2 n))
{-------------------------------------------------------------------------------------------------------------------------------------
----'Is the property hard to test? If you find that it is, can you given a reason why?'

    It's pretty simple, but it turned out that it'll take 'forever' if you don't limit QuickCheck's input value.
    At a max of 20 it runs fine, at >30 it's significantly slow.
    The funcion itself grows exponential with n.


----'Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually?
----Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification?
----Or are you testing something else still?'

  --TODO antwoord

 --------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
3.) Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
----A permutation of a list is a reordering of the members of a list.
----You can also use the Data.List function permutations.
----Find a formula (closed form) for the number of permutations of a list of n distinct objects, and prove your guess by induction.
--------------------------------------------------------------------------------------------------------------------------------------}

test3 = quickCheckResult (\n -> (n >= 0) -->
                         (length(permutations[1..n]) == product[1..n]))

{-------------------------------------------------------------------------------------------------------------------------------------
----'Is the property hard to test? If you find that it is, can you given a reason why?'
    Yes, the function like in Excercise 2 is grows exponential with n.

----Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually?
----Are you checking a mathematical fact? Or are you testing whether perms satisfies a part of its specification?
----Or are you testing something else still?
    TODO antwoord
--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
4.) The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime.
    Write a function that finds all primes < 10000 with this property.
--------------------------------------------------------------------------------------------------------------------------------------}

reversal :: Integer -> Integer
reversal = read . reverse . show
{--
By defenition '-->' is only returns False if the first condition is True and
the second function is False. Since reversal reverses wrong on negative Integers
and Intergers ending on a zero, 1000 reverses in 1. But since all Integers
ending with a zero are even numbers they can't be primes and primes can't be
negative these are automatically returning True. That's why the test
always passes which approves this statement.
--}

checkReversal :: Integer -> Bool
checkReversal = \n -> prime n --> (reversal(reversal n) == n)

test4Reversal = quickCheck (checkReversal)

primesEqReversal :: Integer -> [Integer]
primesEqReversal n = takeWhile ( < n ) (filter (prime . reversal) primes)

-- prime_eq_reverse :: Integer -> Bool
-- prime_eq_reverse n = (prime n) == (prime (reverseInt n))

{-------------------------------------------------------------------------------------------------------------------------------------
    'How would you test this function, by the way?'
    I would want to check whether every number with at least two digits that reversiblePrimes returns has a buddy.
    This could be done by checking for reversals in the list, or possibly even faster when iterating through the
    list x the list, though Haskell's lists appear to be singly-linked and queue-like (because you can easily take
    heads) so it takes a full iteration to reach the last element.
    This isn't suitable for a QuickCheck test because this isn't about testing against random values, we're using
    the same subset of the set of all primes < 10000 each time. Also, by limiting the testset to primes we also don't have to
    check for negative and exponentials of 10
--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
5.) The number 101 is a prime, and it is also the sum of five consecutive primes, namely 13+17+19+23+29.
    Find the smallest prime number that is a sum of 101 consecutive primes.
--------------------------------------------------------------------------------------------------------------------------------------}

sublistPrimes :: [Integer] -> Bool
sublistPrimes n = prime (sum n)

recursiveReturnSublist :: Int -> [a] -> [[a]]
recursiveReturnSublist n l = (take n l) : recursiveReturnSublist n (tail l)

test5 = sum (head (filter sublistPrimes (recursiveReturnSublist 101 primes)))

{-------------------------------------------------------------------------------------------------------------------------------------
----'Do you have to test that your answer is correct? How could this be checked?'
  -- Normally you test all the functions that are created and have some kind of logic in them. In this case the function uses library functions
  -- to calculate the smallest prime. We don't write tests for the library functions because we assume they work as intented.

--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
6.) Using Haskell to refute a conjecture.
    Write a Haskell function that can be used to refute the following conjecture.
    "If p1,...,pn is a list of consecutive primes starting from 2, then (p1×⋯×pn)+1 is also prime."
    This can be refuted by means of a counterexample, so your Haskell program should generate counterexamples.
--------------------------------------------------------------------------------------------------------------------------------------}
recursiveReturnSublist2 :: Int -> [a] -> [[a]]
recursiveReturnSublist2 n l = (take n l) : recursiveReturnSublist2 (n+1) l

conjecture :: [Integer] -> Bool
conjecture xs = prime ((product xs) + 1)

-- Smallest counterexample according to the test is: product [2,3,5,7,11,13] + 1 == 30031 which is not a primenumber
test6 = head (filter (not.conjecture) (recursiveReturnSublist2 1 primes))

{-------------------------------------------------------------------------------------------------------------------------------------
7.) Implement and test the Luhn Algorithm
    The Luhn algorithm is a formula for validating credit card numbers.
    Give an implementation in Haskell. The type declaration is given.
    This function should check whether an input number satisfies the Luhn formula.
--------------------------------------------------------------------------------------------------------------------------------------}
--    luhn :: Integer -> Bool
-- SOURCE digs: https://stackoverflow.com/questions/3989240/int-int-convert
digs :: Integer -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

oddsIndex :: [a] -> [a]
oddsIndex [] = []
oddsIndex [x] = [x]
oddsIndex (e1:e2:xs) = e1 : oddsIndex xs

evenIndex [] = []
evenIndex [x] = []
evenIndex (e1:e2:xs) = (if e2 * 2 > 9 then 2 * e2 - 9 else  2 *e2)
                        : evenIndex xs

indexNum n = sum (oddsIndex( n )) + sum (evenIndex( n ))

luhn :: Integer -> Bool
luhn n = ((indexNum (reverse (digs n))) `mod` 10)  == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isMaster n = length(digs n) == 16 &&
             n1 == 5 &&
             elem n2 [1..5] &&
             luhn n
             where (n1:n2:ns) = digs n

isVisa n = n1 == 4 &&
           (len == 13 || len == 16 || len == 19) &&
           luhn n
           where (n1:ns) = digs n; len = length(digs n)

isAmericanExpress n = length(digs n) == 15 &&
                      n1 == 3 &&
                      (n2 == 4 || n2 == 7) &&
                      luhn n
                      where (n1:n2:ns) = digs n

-- SOURCE : http://www.getcreditcardnumbers.com/
visaValid, masterValid, americaValid :: [Integer]
visaValid = [4532530353861276, -- validCases
             4539091191338186,
             4532660285963153,
             4485675506393284,
             4716050923148342,
             346676877003012, -- inValidCases
             348288126992371,
             379346372653337,
             372551003187871,
             377701033813435]

masterValid = [5561182643231042, -- validCases
               5344239834935031,
               5487320019367914,
               5475210985327105,
               5357091271641828,
               4532530353861276, -- invalidCases
               4539091191338186,
               4532660285963153,
               4485675506393284,
               4716050923148342]

americaValid = [346676877003012, -- validCases
                348288126992371,
                379346372653337,
                372551003187871,
                377701033813435,
                5561182643231042, -- invalidCases
                5344239834935031,
                5487320019367914,
                5475210985327105,
                5357091271641828]

test7Visa, test7Master, test7America :: Bool
test7Visa = (map isVisa visaValid) == validAnswers
test7Master = map isMaster masterValid == validAnswers
test7America = map isAmericanExpress americaValid == validAnswers

validAnswers :: [Bool]
validAnswers = [True,True,True,True,True,False,False,False,False,False] -- test7Visa, test7Master and test7VAmerica should result in this list.
-- changing numbers: https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell

{-------------------------------------------------------------------------------------------------------------------------------------
8.) Use Haskell to write a function that computes who was the thief, and a function that computes which boys made honest declarations.
    Here are some definitions to get you started.
--------------------------------------------------------------------------------------------------------------------------------------}

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses x y | x == Matthew = ((y /= Carl) && (y/= Matthew))
            | x == Peter = (y == Jack) || (y == Matthew)
            | x == Jack = not ((accuses Matthew y) || (accuses Peter y))
            | x == Arnold = ((accuses Peter y) /= (accuses Matthew y))
            | x == Carl = not (accuses Arnold y)

accusers :: Boy -> [Boy]
accusers x = [y | y <- boys, accuses y x]

-- Three people are honest and two aren't. Since all boys where
-- caught in the crime, three (honest) of them will accuse the one
-- who comitted the crime. So the boy who is accused by exactly three
-- other boys actually comitted the crime. The boys who accused the
-- guilty boy are the honest ones.
guilty, honest :: [Boy]
guilty = filter (\x -> length( accusers x) == 3) boys
honest = accusers (head(guilty))

printHonest = print honest
printGuilty = print guilty
