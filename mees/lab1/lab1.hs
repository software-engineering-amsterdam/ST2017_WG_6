module Lab1 where
import Data.List
import Test.QuickCheck


-- ##########
-- # Given
-- ##########
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

choose1 :: Int -> Int -> Int
choose1 n 0 = 1
choose1 0 k = 0
choose1 n k = choose1 (n-1) (k-1) * n `div` k 

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


-- ##########
-- # Lab 1 Exercise 6
-- # 1 hour(s)
-- ##########

generate_n_primes :: Int-> [[Integer]]
generate_n_primes n = (take n primes) : generate_n_primes (n + 1)

inc n = n + 1

sum_primes = product( head( filter (not.prime.inc.product) (generate_n_primes (1)))) + 1


-- ##########
-- # Lab 1 Exercise 7
-- # 2 hour(s)
-- ##########

-- SOURCE digs: https://stackoverflow.com/questions/3989240/int-int-convert
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

odds_index [] = []
odds_index [x] = [x]
odds_index (e1:e2:xs) = e1 : odds_index xs

even_index [] = []
even_index [x] = []
even_index (e1:e2:xs) = (if e2 * 2 > 9 then 2 * e2 - 9 else  2 *e2) 
                        : even_index xs

index_num n = sum (odds_index( n )) + sum (even_index( n )) 

luhn :: Integer -> Bool
luhn n = ((index_num (reverse (digs n))) `mod` 10)  == 0

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
visaValid = [4532530353861276,
             4539091191338186,
             4532660285963153,
             4485675506393284,
             4716050923148342]

masterValid = [5561182643231042,
               5344239834935031,
               5487320019367914,
               5475210985327105,
               5357091271641828]

americaValid = [346676877003012,
                348288126992371,
                379346372653337,
                372551003187871,
                377701033813435]

visa = map isVisa visaValid
master = map isMaster masterValid
america = map isAmericanExpress americaValid
-- changing numbers: https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell

-- ##########
-- # Lab 1 Exercise 8
-- # 2 hour(s)
-- # Results : Jack
-- ##########

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
guilty :: [Boy]
guilty = filter (\x -> length( accusers x) == 3) boys
honest = accusers (head(guilty))

print_honest = print honest
print_guilty = print guilty

-- ##########
-- # Bonus: Euler 10
-- # 0.75 hour(s)
-- # Result -> sum_primes_below 2000000 : 142913828922
-- ##########

-- Might take a few seconds in intepreted mode
sum_primes_below x = foldl (+) 0 $ takeWhile (\n -> n < x) primes

result_euler_10 = print (sum_primes_below 2000000)

-- ##########
-- # Bonus: Euler 49
-- # 2,5 hour(s)
-- # Result ->
-- ##########

primes_between a b  =  filter prime [a .. b]
euler_49 = [[x, y, z] | x <- primes_between 1000 10000,
                        y <- primes_between (x+1) 10000,
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


result_euler_49 = print (joiner (head euler_49))