{-
Time spend : 5 hours 
-}
import Lecture6
import Data.Char

-- To be fair I think this exercise missed its purpose. There is no
-- mentioning in the exercise of the RSA functions in Lecture6.hs 
-- which made me at first create my own implementation 
-- (mees/lab6/Ex7.hs) which made me explore RSA and the workings. After
-- discovering the functions in the Lecture6.hs file it is merely 
-- converting a list of chars to a single int vice versa.

-- Concerning the same bit length I couldn't find and prove that this is
-- required for RSA, And as far as the standard (PKCS#1) for RSA
-- (https://tools.ietf.org/html/rfc3447) it it not required to have the
-- same length and just states that the smallest prime should not be
-- "too small", therefore I do think it is not actually required to be
-- implemented, but we just have to use not "too small" primes.

-- Convert a list of chars to a single Int. To be able to convert
-- it back to a string we make use of the fact to ASCII is represented
-- by a number beteween 0 and 127 (or extended 0 - 255) and thus has at 
-- most 3 digits. We concatenate all numbers and append a zero if
-- the length is lower than 3, otherwise we could not distinguish
-- 122 and 90 when converting it back (12290 could be 12 and 290 
-- or 122 and 90 thus is non deterministic). By appending a zero we 
-- get 122090 which if split on 3 characters is deterministic.

strToInt :: [Char] -> Integer
strToInt x = read (intStr x) :: Integer
    where 
        intStr [] = []
        intStr (xh:xt) = (to3Digits (show (ord (xh)))) ++ intStr xt
        to3Digits x | ((length x) - 3) == 0 = x
                    | otherwise = to3Digits ("0" ++ x)

intToStr :: Show a => a -> [Char]
intToStr x = strInt (splitAt 3 (reverse (show x)))
    where
        strInt ("",_) = []
        strInt (x ,y) = strInt (splitAt 3 y) ++ [chr(read (reverse x) :: Int)]

showRSA :: [Char] -> Integer -> Integer -> IO ()
showRSA msg p q = do
  let (pubKey, privKey) = (rsaPublic p q, rsaPrivate p q)
  let encMsg = rsaEncode privKey (strToInt msg)
  print ("Encoded message: " ++ show encMsg)
  print (intToStr (rsaDecode pubKey encMsg))

-- Input two high primes, the 12th and 13th mersene prime.
main = showRSA message (mers 12) (mers 13)














































message = "Whoooooeeeeeehoooeee, have a nice day!"