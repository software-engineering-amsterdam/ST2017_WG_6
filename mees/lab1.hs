module Lab1 where
import Data.List
import Test.QuickCheck



-- ##########
-- # Lab 1 Exercise 1 
-- # 5 hour(s)
-- ##########

fac2 :: Int -> Int
fac2 0 = 0
fac2 n = n ^ 2 + fac2 (n - 1)

proof2:: Int -> Int
proof2 n = quot (n * (n + 1) * (2 * n + 1)) 6


ex_1 :: Int -> Bool
ex_1 n = fac2 n == proof2 n


(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

test11 = quickCheckResult (\n -> (n >= 0) --> (fac2 n == proof2 n))

fac3 :: Int -> Int
fac3 0 = 0
fac3 n = n ^ 3 + fac3 (n - 1)

proof3 :: Int -> Int
proof3 n = (n * (n + 1) `div` (2)) ^ 2


test12 = quickCheckResult (\n -> (n >= 0) --> (fac3 n == proof3 n))

-- ##########
-- # Lab 1 Exercise 2
-- # 1,5 hour(s)
-- ##########

sub_eq_n2 :: Int -> Bool
sub_eq_n2 n = length(subsequences[1..n]) == 2 ^ n

test2 = quickCheckResult (\n -> (n >= 0) --> (sub_eq_n2 n))

-- ##########
-- # Lab 1 Exercise 3
-- # 0.5 hour(s)
-- ##########

test3 = quickCheckResult (\n -> (n >= 0) -->
                         (length(permutations[1..n]) == product[1..n]))

-- ##########
-- # Lab 1 Exercise 4
-- # 2 hour(s)
-- ##########

-- SOURCE : https://blackboard.uva.nl/webapps/blackboard/execute/content/file?cmd=view&content_id=_6832804_1&course_id=_212568_1&framesetWrapped=true
-- 'reversal' Looks broken, for instance 100 gives 1 and minus integers are failing
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
--check_reversal :: Integer -> Bool
check_reversal = \n -> prime n --> (reversal(reversal n) == n)

test4_reversal = quickCheck (check_reversal)


prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primes_eq_reversal :: Integer -> [Integer]
primes_eq_reversal n = takeWhile ( < n ) (filter (prime . reversal) primes)  

-- prime_eq_reverse :: Integer -> Bool
-- prime_eq_reverse n = (prime n) == (prime (reverseInt n))

-- ##########
-- # Lab 1 Exercise 5
-- # 1,5 hour(s)
-- ##########
{--
--}
-- function l a = (filter (prime.sum))

generate_n_head :: Int -> [a] -> [[a]]
generate_n_head n l = (take n l) : generate_n_head n (tail l)

sum_head_n = sum( head( filter (prime.sum) (generate_n_head 101 primes)))