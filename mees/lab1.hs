module Lab1 where
import Data.List
import Test.QuickCheck



-- ##########
-- # Exercise 1 
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

test1 = quickCheckResult (\n -> (n >= 0) --> (fac2 n == proof2 n))

fac3 :: Int -> Int
fac3 0 = 0
fac3 n = n ^ 3 + fac3 (n - 1)

proof3 :: Int -> Int
proof3 n = (n * (n + 1) `div` (2)) ^ 2


test2 = quickCheckResult (\n -> (n >= 0) --> (fac3 n == proof3 n))
-- ex_2 = 


-- # https://mail.haskell.org/pipermail/beginners/2011-May/007122.html
-- \
-- fac3 0 = 0
-- fac3 n = n ^ 3 + fac3 (n - 1)

-- proof3 n = (quot (n * (n + 1) (2))) ^ 2


    
-- p xs -> all p xs == myall p xs

-- prime :: Integer -> Bool
-- prime n = n > 1 && all (\ x -> rem n x /= 0) xs
--   where xs = takeWhile (\ y -> y^2 <= n) primes
-- primes :: [Integer]
-- primes = 2 : filter prime [3..] 
-- a = "abs"

-- myall :: (a -> Bool) -> [a] -> Bool
-- myall p [] = True
-- myall p (x:xs) = p x && myall p xs

-- list2p :: Eq a => [a] -> a -> Bool
-- list2p = flip elem

-- myallTest :: [Int] -> [Int] -> Bool
-- myallTest = \ ys xs -> let p = list2p ys in
--  all p xs == myall p xs