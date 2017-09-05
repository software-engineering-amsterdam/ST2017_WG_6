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

-- num_perms_eq :: Int -> Bool
-- num_perms_eq n = length(permutations[1..n]) == product[1..n]

test3 = quickCheckResult (\n -> (n >= 0) -->
                         (length(permutations[1..n]) == product[1..n]))

-- ##########
-- # Lab 1 Exercise
-- # hour(s)
-- ##########
