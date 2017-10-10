
module Lecture6

where

import System.Random

{-
    Assignment:		Lab 6: Exercise 1
    Name:           Sara Oonk
    Time spent:
    Sources:        Modular exponentiation:             https://en.wikipedia.org/wiki/Modular_exponentiation
                    Power of two:                       https://en.wikipedia.org/wiki/Power_of_two
                    How to check if power of two:       http://www.exploringbinary.com/ten-ways-to-check-if-an-integer-is-a-power-of-two-in-c/
                    Modular Exponentiation Calculator:  http://comnuan.com/cmnn02/cmnn02008/
    Comments:


Exercise 1

Implement a function

 exM :: Integer -> Integer -> Integer -> Integer
that does modular exponentiation of xy in polynomial time, by repeatedly squaring modulo N.

E.g., x33 mod5 can be computed by means of

x33(mod5)=x32(mod5)×x(mod5).

x32(modN) is computed in five steps by means of repeatedly squaring modulo N:

x(modN)→x2(modN)→x4(modN)→…→x32(modN).

If this explanation is too concise, look up relevant literature


-}


{- exM: Modular exponentiation of b to the power of e by repeatedly squaring modulo m.
 - Integer b: base
 - Integer e: exponent
 - Integer m: modulus
 -}
exM :: Integer -> Integer -> Integer -> Integer
exM b e m | isPowerOfTwo(e) = exMr b e m
          | otherwise = exMl intermediate m (e-npt)
                             where npt          = (findNearestPowerOfTwo e)
                                   intermediate = (exMr b npt m 2)

-- (e-npt) is the difference between the actual goal exponent and the nearest power-of-two exponent,
-- in other words the number of multiplications that remain to be done.

-- (b^2): we're starting off at the first squaring so that we can pass 2 as next exponent,
-- this way recursion can take over by squaring 2.



{- exMr: Recursively square values until exponent e has been reached by goal g.
 - Integer b: base
 - Integer g: goal exponent
 - Integer m: modulo
 - Integer e: next exponent
 -}
exMr :: Integer -> Integer -> Integer -> Integer -> Integer
exMr b e m | (g == 1) = (b `mod` m)
             | (g == e) = b
             | (e < g)  = exMr (multM (b `mod` m) (b `mod` m) m) g m (e^2)
             | (e > g)  = undefined

{- exmL: Multiply for the leftover amount of exponents
 - b: base
 - m: modulus
 - l: leftover
 -}
exMl :: Integer -> Integer -> Integer -> Integer
exMl b m l = if l == 0 then (b `mod` m) else exMl (multM (b `mod` m) (b `mod` m) m) m (l-1)


isPowerOfTwo :: Integer -> Bool
isPowerOfTwo n = if (odd n) then (n == 1) else isPowerOfTwo (n `div` 2)

findNearestPowerOfTwo :: Integer -> Integer
findNearestPowerOfTwo n = if (isPowerOfTwo n) then n else findNearestPowerOfTwo (n-1)



---

factorsNaive :: Integer -> [Integer]
factorsNaive n0 = factors' n0 2 where
  factors' 1 _ = []
  factors' n m
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n0 = let
   ps0 = takeWhile (\ m -> m^2 <= n0) primes
 in factors' n0 ps0 where
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps)
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

prime :: Integer -> Bool
prime n = factors n == [n]

primes :: [Integer]
primes = 2 : filter prime [3..]

mers :: Integer -> Integer
mers 1  = 2^2-1;    mers 2  = 2^3-1;     mers 3  = 2^5-1
mers 4  = 2^7-1;    mers 5  = 2^13-1;    mers 6  = 2^17-1
mers 7  = 2^19-1;   mers 8  = 2^31-1;    mers 9  = 2^61-1
mers 10 = 2^89-1;   mers 11 = 2^107-1;   mers 12 = 2^127-1
mers 13 = 2^521-1;  mers 14 = 2^607-1;   mers 15 = 2^1279-1
mers 16 = 2^2203-1; mers 17 = 2^2281-1;  mers 18 = 2^3217-1
mers 19 = 2^4253-1; mers 20 = 2^4423-1;  mers 21 = 2^9689-1
mers 22 = 2^9941-1; mers 23 = 2^11213-1; mers 24 = 2^19937-1
mers 25 = 2^21701-1;
mers _  = undefined

bin2int :: [Int] -> Int
bin2int = bin . reverse where
  bin []  = 0
  bin [0] = 0
  bin [1] = 1
  bin (0:bs) = 2 * bin bs
  bin (1:bs) = 2 * bin bs + 1
  bin _      = error "not a binary digit list"

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y)

invM :: Integer -> Integer -> Integer
invM x n = let
   (u,v) = fctGcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n
 in
   if copr then i else error "no inverse"

fGcd :: Integer -> Integer -> Integer
fGcd a b = if b == 0 then a
                     else fGcd b (rem a b)

fctGcd :: Integer -> Integer -> (Integer,Integer)
fctGcd a b =
  if b == 0
  then (1,0)
  else
     let
       (q,r) = quotRem a b
       (s,t) = fctGcd b r
     in (t, s - q*t)

coprime :: Integer -> Integer -> Bool
coprime n m = fGcd n m == 1

coprime' :: Integer -> Integer -> Bool
coprime' n m = let (x,y) = fctGcd n m
               in x*n + y*m == 1

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

takeT :: Int -> Tree a -> Tree a

takeT 0 (T x _) = T x []
takeT n (T x ts) = T x (map (takeT (n-1)) ts)

coprimeT :: Tree (Integer,Integer)
coprimeT = grow f (1,1)

f :: (Integer,Integer) -> [(Integer,Integer)]
f (n,m) = [(n+m,m),(n,n+m)]

pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

--exM :: Integer -> Integer -> Integer -> Integer
--exM = expM -- to be replaced by a fast version

primeTestF :: Integer -> IO Bool
primeTestF n = do
   a <- randomRIO (2, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

decomp :: Integer -> (Integer,Integer)
decomp n0 = decomp' (0,n0) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

mrComposite :: Integer -> Integer -> Bool
mrComposite x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1)
       (map (\ j -> exM x (2^j*s) n)  [0..r])
  in
    exM x s n /= 1 && last fs /= (n-1)

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n

composites :: [Integer]
composites = error "not yet implemented"

encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = m*k `mod` p

decodeDH :: Integer -> Integer -> Integer -> Integer -> Integer
decodeDH p ga b c = let
    gab' = exM ga ((p-1)-b) p
  in
    rem (c*gab') p

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 in
   exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p'
 in
   exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

totient :: Integer -> Integer
totient n = toInteger $ length [ k | k <- [1..n], gcd k n == 1 ]

phi :: Integer -> Integer -> Integer
phi p q = (p - 1) * (q - 1)

select :: Integer -> Integer -> Integer
select p q = let
   t = phi p q
 in
   head [ x | x <- [3..], gcd x t == 1 ]

rsaPublic :: Integer -> Integer -> (Integer,Integer)
rsaPublic p q = let
    e = select p q
  in
    (e,p*q)

rsaPrivate ::  Integer -> Integer -> (Integer,Integer)
rsaPrivate p q = let
   e = select p q
   t = phi p q
   d = invM e t
  in
   (d,p*q)

rsaEncode :: (Integer,Integer) -> Integer -> Integer
rsaEncode (e,n) m =  exM m e n

rsaDecode :: (Integer,Integer) -> Integer -> Integer
rsaDecode = rsaEncode

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsaEncode

secret, bound :: Integer
secret = mers 18
bound  = 131


