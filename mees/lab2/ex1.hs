-- ##########
-- # https://blackboard.uva.nl/bbcswebdav/pid-6839934-dt-content-rid-11209217_1/courses/2318N001.5364SOTE6Y.S1.1.2017/Lab2.html
-- # Exercise 1
-- # 4,5 hours
-- # Result -> 
-- ##########
-- # Given :
-- ##########

import Data.List
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)

-- ##########
-- # Self coded
-- ##########

upNth t (x:xs) 
     | t < 0.25 = (x + 1):xs
     | otherwise = x:upNth (t - 0.25) xs

main = do
    x <- probs 10000
    print (foldr (zipWith (+)) [0,0,0,0] (map (\x -> upNth x [0,0,0,0]) x))

-- ########## TODO TODO TODO TODO ##########
-- #                                       #
-- #                 Tests                 #
-- #                                       #
-- #########################################