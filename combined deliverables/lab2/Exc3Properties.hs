
module Exc3Properties where
import System.Random
import Test.QuickCheck
import Lecture2

{-------------------------------------------------------------------------------------------------------------------------------------
3 a)  Implement all properties from the Exercise 3 from Workshop 2 as
    Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10][(−10)..10].

    (\ x -> even x && x > 3) even
    (\ x -> even x || x > 3) even
    (\ x -> (even x && x > 3) || even x) even
    even (\ x -> (even x && x > 3) || even x)

    Time spent: 3h
--------------------------------------------------------------------------------------------------------------------------------------}

property1, property2, property3 :: Int -> Bool
property1 = (\ x -> even x && x > 3)
property2 = (\ x -> even x || x > 3)
property3 = (\ x -> (even x && x > 3) || even 3)

-- Which of the following properties is stronger? assume domain [1..10]
compare1 = compar [1..10] property1 even -- Result property1 (p1) is stronger than even. p1 is stronger because it is a subset of even.
-- p1 [x <-x mod 2 == 0, x > 3] and even = [x <- x mod 2 == 0, x > 3 || x =< 3]
compare2 = compar [1..10] property2 even -- Result property2 is weaker than even
compare3 = compar [1..10] property3 even -- Result property3 is stronger than even
compare4 = compar [1..10] even property3 -- Result even is weaker than property3


{-------------------------------------------------------------------------------------------------------------------------------------
b)  Provide a descending strength list of all the implemented properties.
--------------------------------------------------------------------------------------------------------------------------------------}
-- For convience I made a list of tuples containing all the properties with an int equivalent to the property
-- I did this so I can print the int in a list
allProperties :: [(Int, Int -> Bool)]
allProperties = [(1, property1), (2, property2), (3, property3), (4, even)]

-- Modified stronger to work with quickSortProperties.compare1
strongerNotEquivalent :: (Int->Bool) -> (Int -> Bool) -> Bool
strongerNotEquivalent p q = pq && not(qp) where
                                pq = stronger [(-10)..10] p q
                                qp = stronger [(-10)..10] q p

-- This custom quicksort sorts the properties based on a modified stronger and weaker and prints the result in the form of an integer list.
quickSortProperties :: [(Int, Int -> Bool)] -> [Int]
quickSortProperties [] = []
quickSortProperties (x:xs) =
   quickSortProperties [ a | a <- xs, strongerNotEquivalent (snd a) (snd x) ]
   ++ [(fst x)]
   ++ quickSortProperties [ a | a <- xs, weaker [(-10)..10] (snd a) (snd x) ]

exercise3 = quickSortProperties allProperties

main = do
  print "Which of the following properties is stronger?"
  print compare1
  print compare2
  print compare3
  print compare4

  print "A descending list strength list of all the implemented properties"
  print exercise3

{-- the result of exercise3 is [1, 3, 4, 2]
    The int in the list correspond to the properties. To check if the list produced by exercise3 is truly correct,
    I checked if result is correct with compar. Which gave me 1 eq 3, 3 stronger than 4 and 4 stronger than 2.

    This also means that:
     1 is stronger than 4 and 2
     3 is eq to 1 and stronger than 2
     etc.

    Which is also true according to compar. In this test we assumed compar, stronger and weaker work as intented
--}
