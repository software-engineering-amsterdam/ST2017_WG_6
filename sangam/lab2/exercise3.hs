
module Exercise3 where
import System.Random
import Test.QuickCheck
import Lecture2

{-------------------------------------------------------------------------------------------------------------------------------------
Helpers
--------------------------------------------------------------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------------------------------------------------------------
a)  Implement all properties from the Exercise 3 from Workshop 2 as
    Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10][(−10)..10].

    Which of the following properties is stronger? assume domain [1..10]

    (\ x -> even x && x > 3) even
    (\ x -> even x || x > 3) even
    (\ x -> (even x && x > 3) || even x) even
    even (\ x -> (even x && x > 3) || even x)

--------------------------------------------------------------------------------------------------------------------------------------}

property1, property2, property3, property4 :: Int -> Bool
property1 = (\ x -> even x && x > 3)
property2 = (\ x -> even x || x > 3)
property3 = (\ x -> (even x && x > 3) || even 3)
property4 = property3
property5 = even

{-------------------------------------------------------------------------------------------------------------------------------------
b)  Provide a descending strength list of all the implemented properties.
--------------------------------------------------------------------------------------------------------------------------------------}
v :: [(Int, Int -> Bool)]
v = [(1, property1), (2, property2), (3, property3), (4, property4), (5, property5)]

p :: [(Int, Int -> Bool)] -> [Int]
p [] = []
p (x:xs) =
   p [ a | a <- xs, s (snd a) (snd x) ]
   ++ [(fst x)]
   ++ p [ a | a <- xs, w (snd a) (snd x) ]

s:: (Int->Bool) -> (Int -> Bool) -> Bool
s a b = stronger [(-10)..10] a b && not (stronger [(-10)..10] b a)

w:: (Int->Bool) -> (Int -> Bool) -> Bool
w a b = (not (stronger [(-10)..10] a b) && stronger [(-10)..10] b a) || (stronger [(-10)..10] a b && stronger [(-10)..10] b a)
