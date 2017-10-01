{-
Assignment:		Lab 4: Bonus Project Euler problem
Name:           Tim Nederveen
Time spent:     4.5h

Remarks:        Answer: The sum of shortest-path multiplications needed for exponents of n^k with k from 1 to 200 is 1582.
Sources:        - https://projecteuler.net/problem=122
                - https://en.wikipedia.org/wiki/Addition-chain_exponentiation
                - 
---------------}

{-
Efficient Exponentiation
Problem 122

The most naive way of computing n15 requires fourteen multiplications:

n × n × ... × n = n15

But using a "binary" method you can compute it in six multiplications:

n × n = n2
n2 × n2 = n4
n4 × n4 = n8
n8 × n4 = n12
n12 × n2 = n14
n14 × n = n15

However it is yet possible to compute it in only five multiplications:

n × n = n2
n2 × n = n3
n3 × n3 = n6
n6 × n6 = n12
n12 × n3 = n15

We shall define m(k) to be the minimum number of multiplications to compute nk; for example m(15) = 5.

For 1 ≤ k ≤ 200, find ∑ m(k).

-}


{-
A subproblem of this is generating an Addition chain. Finding minimum number of multiplications to get the wanted 
exponentiation can be done by the Addition-chain exponentiation method. The shortest addition-chain algorithm requires
at max as much multiplications as binary exponentiation, but usually less, with a^15 being the first example of outperformance 
of binary exponentiation (shortest addition chain needs 5, binary exponentiation needs 6 multiplications).
Determining the shortest addition chain for a given set of exponents has been proven NP-complete, although approximations 
of the shortest addition chain such as exponentiation are available. Dynamic programming also doesn't work as it cannot be 
divided into independent subproblems. The chosen solution uses recursion for a dept-first search approach, and is based on 
addition-chain exponentiation as discussed in https://en.wikipedia.org/wiki/Addition-chain_exponentiation
-}

module Exc10 where

import Data.List

sumOfChainsTo :: Integer -> Integer
sumOfChainsTo x = toInteger $ sum $ map minChain [1..x]

nextSet :: Num b => [b] -> [[b]]
nextSet l = map ((:l) . (+ head l)) l

minChain :: Integer -> Int
minChain x = minChain' [[1]]
    where 
        minChain' l = case find ((==x) . head) l of
            Nothing -> minChain' . concatMap nextSet $ filter ((<x) . head) l
            Just result -> length result - 1

main :: Integer
main = sumOfChainsTo 200