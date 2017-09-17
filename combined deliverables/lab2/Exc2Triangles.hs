{-
Assignment:		Lab 2: ...
Name:           Tim Nederveen
UVA id:      	11959037
Remarks:        --
Sources:        --
-}

module Exc2Triangles where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2


{-------------------------------------------------------------------------------------------------------------------------------------
2.) Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:
- Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
- Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
- Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
- Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,
- Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

Deliverables: Haskell program, concise test report, indication of time spent.
--------------------------------------------------------------------------------------------------------------------------------------}


data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = orderedInputTriangle (sort [x, y, z])

orderedInputTriangle :: [Integer] -> Shape
orderedInputTriangle [x,y,z]
    | x + y <= z = NoTriangle
    | x == y && y == z = Equilateral
    | x^2 + y^2 == z^2 = Rectangular
    | x == y || y == z = Isosceles
    | otherwise = Other


inputs = [ [x,y,z] | x <- [-1..5],
                     y <- [x..5],
                     z <- [y..5] ]


orderIrrelevant :: Integer -> Integer -> Integer -> Bool
orderIrrelevant = \x y z -> (triangle x y z == triangle y z x && triangle x y z == triangle z y x)

multiplicationIrrelevant :: Integer -> Integer -> Integer -> Integer -> Bool
multiplicationIrrelevant = \w x y z -> (triangle x y z == triangle (w*x) (w*y) (w*z))

test1 = quickCheckResult (\x y z -> (x >= 0 && y >= 0 && z >= 0) --> orderIrrelevant x y z)

test2 = quickCheckResult (\w x y z -> (w > 0 && x >= 0 && y >= 0 && z >= 0) --> multiplicationIrrelevant w x y z)

main = do
  test1
  test2
