{-

Assignment:		Lab 2: Exercise 2 - Recognizing triangles
Name:           Tim Nederveen
Time spent:     3.5h

---------------}


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

--------------------------------------------------------------------------------------------------------------------------------------}

{--
    We both perform manual tests using shapes that are known to fit specific triangle categories, 
    and a quickcheck test check if the program correctly handles scrambled input orderings, which should not lead
    to different outcomes. 
--}


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


orderIrrelevant :: Integer -> Integer -> Integer -> Bool
orderIrrelevant = \x y z -> (triangle x y z == triangle y z x && triangle x y z == triangle z y x)

test1 = quickCheckResult (\x y z -> (x >= 0 && y >= 0 && z >= 0) --> orderIrrelevant x y z)

inputs = [ [x,y,z] | x <- [-1..5],
                     y <- [x..5],
                     z <- [y..5] ]


-- inputNoTriangle = [[2,3,8],[20,6,4],[9,1,2]]
-- inputEquilateral = [[2,2,2],[5,5,5],[15,15,15]]
-- inputRectangular = [[3,4,5],[10,6,8],[12,20,16]]
-- inputIsosceles = [[5,5,8],[7,6,6],[3,2,3]]

process :: [Integer] -> Shape -> Bool
process (x:y:z:xs) shape = triangle x y z == shape

manualTests
    | triangle 2 3 8 /= NoTriangle = print "Test NoTriangle failed"
    | triangle 2 2 2 /= Equilateral = print "Test Equilateral failed"
    | triangle 3 4 5 /= Rectangular = print "Test Rectangular failed"
    | triangle 5 5 8 /= Isosceles = print "Test Isosceles failed"
    | otherwise = print "++ All manual set test cases are valid"


main = do
  manualTests
  test1
  -- map process inputNoTriangle NoTriangle
