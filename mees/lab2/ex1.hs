-- ##########
-- # https://blackboard.uva.nl/bbcswebdav/pid-6839934-dt-content-rid-11209217_1/courses/2318N001.5364SOTE6Y.S1.1.2017/Lab2.html
-- # Exercise 1
-- # 4,5 hours
-- # Result -> 
-- ##########
-- # Given : Red Curry is right
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

percQuartiles n l = do
    let m = fromIntegral n
    x <- probs n
    let countedQuartiles = foldr (zipWith (+)) [0,0,0,0] (map (\x -> upNth x l) x)
    print (zipWith (/) countedQuartiles [m,m,m,m])

main = percQuartiles 100000 [0,0,0,0]


-- ########## TODO TODO TODO TODO ##########
-- #                                       #
-- #              Explanation              #
-- #                                       #
-- #########################################
