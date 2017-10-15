module Lab6_4
where
import Lecture6
import Lab6_3


{-
    Assignment:		Lab 6: Exercise 4
    Name:           Sara Oonk
    Time spent:
    Sources:        Using sara/lab6_3.hs

    Comments:


Use the list of composite numbers to test Fermat's primality check.
What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3?
What happens if you increase k?

-}


--recursively check n++ until find false
-- then for k2, 3, etc

findFermatError = [testFermatsCheck (fromInteger n) 1 | n <- composites']



testFermatsCheck :: Int -> Integer -> IO Int
testFermatsCheck n k = do
 fermat <- (primeTestsF n k)
 if (fermat == (primeLec1 (toInteger n))) then testFermatsCheck (n+1) k else return n


testpls = do print findFermatError