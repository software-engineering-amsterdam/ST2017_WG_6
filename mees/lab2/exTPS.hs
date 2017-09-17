-- ##########
-- # https://blackboard.uva.nl/bbcswebdav/pid-6839934-dt-content-rid-11209217_1/courses/2318N001.5364SOTE6Y.S1.1.2017/Lab2.html
-- # Exercise 1
-- # 4,5 hours
-- # Result -> 
-- ##########

import Data.List
import System.Random
import Lecture2

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 = (\ x -> even x && x > 3)
prop2 = (\ x -> even x || x > 3)
prop3 = (\ x -> (even x && x > 3) || even x)
prop4 = even



-- Which of the following properties is stronger? assume domain [1..10]

-- (\ x -> even x && x > 3) even
-- (\ x -> even x || x > 3) even
-- (\ x -> (even x && x > 3) || even x) even
-- even (\ x -> (even x && x > 3) || even x)