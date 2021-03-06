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

    Implementation by:      Sara
    Arguments for choice:   Readability
    Time spent:             1h 30m

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

test1 = quickCheckResult (\n -> n >= 0 --> exercise3a n == exercise3b n)

{-------------------------------------------------------------------------------------------------------------------------------------
2.) Redo exercise 4 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
    You can use subsequences :: [a] -> [[a]] for the list of all subsequences of a given list.

    'Prove by induction that if A is a finite set with |A|=n, then |P(A)|=2n.'

    Implementation by:      Mees
    Arguments for choice:   Most compact
    Time spent:             1h 30m
--------------------------------------------------------------------------------------------------------------------------------------}
subEqn2 :: Int -> Bool
subEqn2 n = length(subsequences[1..n]) == 2 ^ n

test2 = quickCheckResult (\n -> (n >= 0 && n < 20) --> (subEqn2 n))
{-------------------------------------------------------------------------------------------------------------------------------------
----'Is the property hard to test? If you find that it is, can you given a reason why?'

    Yes the property is hard to test because the function itself grows exponential with n.

----'Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually?
----Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification?
----Or are you testing something else still?'

    We are testing a specification. But only a small part. To test it correctly we would also need to check if the value of the result of
    subsequences is also equal to its definition. E.g. subsequences[1..3] should result in [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

 --------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
3.) Redo exercise 5 of Workshop 1 by replacing sets by lists, and testing the property for integer lists of the form [1..n].
    A permutation of a list is a reordering of the members of a list.
    You can also use the Data.List function permutations.
    Find a formula (closed form) for the number of permutations of a list of n distinct objects, and prove your guess by induction.

    Implementation by:      Mees
    Arguments for choice:   Compact and consistent with notation of previous exercise
    Time spent:             30m

--------------------------------------------------------------------------------------------------------------------------------------}

test3 = quickCheckResult (\n -> (n >= 0) -->
                         (length(permutations[1..n]) == product[1..n]))

{-------------------------------------------------------------------------------------------------------------------------------------
----'Is the property hard to test? If you find that it is, can you given a reason why?'
    Yes, the function like in Exercise 2 is grows exponential with n.

----Again, give your thoughts on the following issue: when you perform the test for exercise 5, what are you testing actually?
----Are you checking a mathematical fact? Or are you testing whether perms satisfies a part of its specification?
----Or are you testing something else still?

    Same answer as 2. We are testing a specification. But only a small part. To test it correctly we would also need to check if the value of the result of
    permutations is also equal to its definition. E.g. permutations[1..3] should result in [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]

--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
4.) The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime.
    Write a function that finds all primes < 10000 with this property.

    Implementation by:      Mees
    Arguments for choice:   Compact
    Time spent:             2h
--------------------------------------------------------------------------------------------------------------------------------------}

reversal :: Integer -> Integer
reversal = read . reverse . show
{--
By defenition '-->' is only returns False if the first condition is True and
the second function is False. Since reversal reverses wrong on negative Integers
and Intergers of base 10, 1000 reverses in 1. But since all Integers
that are base 10 are even numbers they can't be primes and primes can't be
negative these are automatically returning True. That's why the test
always passes which approves this statement.
--}

checkReversal :: Integer -> Bool
checkReversal = \n -> prime n --> (reversal(reversal n) == n)

test4Reversal = quickCheckResult (checkReversal)

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
    the same subset of the set of all primes < 10000 each time.
--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
5.) The number 101 is a prime, and it is also the sum of five consecutive primes, namely 13+17+19+23+29.
    Find the smallest prime number that is a sum of 101 consecutive primes.

    Implementation by:      Tim
    Arguments for choice:   Readability
    Time spent:             1h 30m

--------------------------------------------------------------------------------------------------------------------------------------}

sublistPrimes :: [Integer] -> Bool
sublistPrimes n = prime (sum n)

recursiveReturnSublist :: Int -> [a] -> [[a]]
recursiveReturnSublist n l = (take n l) : recursiveReturnSublist n (tail l)

test5 = sum (head (filter sublistPrimes (recursiveReturnSublist 101 primes)))

{-------------------------------------------------------------------------------------------------------------------------------------
    'Do you have to test that your answer is correct? How could this be checked?'

     Normally you test all the functions that are created and have some kind of logic in them. In this case the function uses library functions
     to calculate the smallest prime. We don't write tests for the library functions because we assume they work as intended.

--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
6.) Using Haskell to refute a conjecture.
    Write a Haskell function that can be used to refute the following conjecture.
    "If p1,...,pn is a list of consecutive primes starting from 2, then (p1×⋯×pn)+1 is also prime."
    This can be refuted by means of a counterexample, so your Haskell program should generate counterexamples.

    Implementation by:      Tim
    Arguments for choice:   Most easily understandable
    Time spent:             1h
--------------------------------------------------------------------------------------------------------------------------------------}
recursiveReturnSublist2 :: Int -> [a] -> [[a]]
recursiveReturnSublist2 n l = (take n l) : recursiveReturnSublist2 (n+1) l

conjecture :: [Integer] -> Bool
conjecture xs = prime ((product xs) + 1)

-- Smallest counterexample according to the test is: product [2,3,5,7,11,13] + 1 == 30031 which is not a prime number
test6 = head (filter (not.conjecture) (recursiveReturnSublist2 1 primes))

{-------------------------------------------------------------------------------------------------------------------------------------
7.) Implement and test the Luhn Algorithm
    The Luhn algorithm is a formula for validating credit card numbers.
    Give an implementation in Haskell. The type declaration is given.
    This function should check whether an input number satisfies the Luhn formula.

    Implementation by:      Mees
    Arguments for choice:   Most complete, well-structured and well-tested
    Time spent:             2h
--------------------------------------------------------------------------------------------------------------------------------------}
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
-- changing numbers: https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell

validAnswers :: [Bool]
validAnswers = [True,True,True,True,True,False,False,False,False,False] -- test7Visa, test7Master and test7VAmerica should result in this list.

{-------------------------------------------------------------------------------------------------------------------------------------
8.) Use Haskell to write a function that computes who was the thief, and a function that computes which boys made honest declarations.
    Here are some definitions to get you started.

    Implementation by:      Mees
    Arguments for choice:   Readability
    Time spent:             2h
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

{-------------------------------------------------------------------------------------------------------------------------------------
 Bonus Euler 9.)
 Implementation by:      Sara
 Arguments for choice:   Specific experience (from 'Programming in Haskell') resulting in clean one-line solution
 Time spent:             20m

 Result -> 31875000
--------------------------------------------------------------------------------------------------------------------------------------}
-- Take head only because otherwise it will a.) keep searching after finding the triplet
-- and b.) return a,b,c and also b,a,c which is unnecessary
-- Limit the lists to 500 to prevent infinite loop
-- Finding the triplet takes a moment :)
pythagoreanTripletSums1000 :: Int
pythagoreanTripletSums1000 = product(head [[x,y,z]| x <- [1..500],
                                                    y <- [x..500],
                                                    z <- [y..500],
                                                    (x^2 + y^2 == z^2),
                                                    x + y + z == 1000])

{-------------------------------------------------------------------------------------------------------------------------------------
 Bonus Euler 10.)
 Implementation by:      Mees
 Arguments for choice:   Readability
 Time spent:             45m

 Result -> sum_primes_below 2000000 : 142913828922
--------------------------------------------------------------------------------------------------------------------------------------}
-- Might take a few seconds in interpreted mode
sumPrimesBelow :: Integer -> Integer
sumPrimesBelow x = foldl (+) 0 $ takeWhile (\n -> n < x) primes

resultsEuler10 = print (sumPrimesBelow 2000000)

{-------------------------------------------------------------------------------------------------------------------------------------
 Bonus Euler 49.)
 Implementation by:      Mees
 Arguments for choice:   Similar to Sara's solution, but easier on the eyes
 Time spent:             2h 30m

 Result -> 296962999629
--------------------------------------------------------------------------------------------------------------------------------------}
primesBetween :: Integer -> Integer -> [Integer]
primesBetween a b  =  filter prime [a .. b]

euler49 :: [[Integer]]
euler49 = [[x, y, z] | x <- primesBetween 1000 10000,
                       y <- primesBetween (x+1) 10000,
                       z <- [y + (y - x)],
                       z < 10000,
                       x /= 1487,
                       prime z,
                       (digs y) `elem` (permutations(digs x)),
                       (digs z) `elem` (permutations(digs x))
                       ]

-- SOURCE: https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines
joiner :: [Integer] -> Integer
joiner = read . concatMap show


resultEuler49 = print (joiner (head euler49))
