-- ##########
-- # https://blackboard.uva.nl/bbcswebdav/pid-6839934-dt-content-rid-11209217_1/courses/2318N001.5364SOTE6Y.S1.1.2017/Lab2.html
-- # Exercise 1
-- # 4,5 hours
-- # Result -> Red Curry is right
-- ##########
-- # Given 
-- ##########
module Exc1RedCurry where
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

main = percQuartiles 10000 [0,0,0,0]


-- ##################
-- #                #
-- #  Explanation   #
-- #                #
-- ##################

-- percQuartiles 10 ->       [0.3      ,0.2     ,0.3      ,0.2]
-- percQuartiles 100 ->      [0.26     ,0.2     ,0.27     ,0.27]
-- percQuartiles 1000 ->     [0.253    ,0.251   ,0.243    ,0.253]
-- percQuartiles 10000 ->    [0.2503   ,0.2467  ,0.2434   ,0.2596]
-- percQuartiles 100000 ->   [0.24677  ,0.25126 ,0.25281  ,0.24916]
-- percQuartiles 1000000 ->  [0.249381 ,0.250212,0.250519 ,0.249888]
-- percQuartiles 10000000 -> [0.2498527,0.250118,0.2500285,0.2500008]

-- All quartiles converge to 0.25
