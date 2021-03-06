{--
Assignment:     Lab 5: Assignment 1
Name:           Sangam Gupta
Time spent:     3 hours
--}

module Lab5 where

import Data.List
import System.Random
import Lecture5

{- 
Below the modified code in lecture5.hs

bl1 :: Int -> [Int]
bl1 x = concat $ filter (elem x) blocks1

blocks1 :: [[Int]]
blocks1 = [[2..4],[6..8]]

subGrid1:: Sudoku -> (Row, Column) -> [Value]
subGrid1 s (r,c) = [ s (r',c') | r' <- bl1 r, c' <- bl1 c ]

freeInSubgrid1 :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid1 s (r,c) = freeInSeq (subGrid1 s (solveAr,c))

sameblock1 :: (Row,Column) -> (Row,Column) -> Bool
sameblock1 (r,c) (x,y) = bl1 r == bl1 x && bl1 c == bl1 y 
-}

test = do n <- return (initNode example6)
          solveShowNs n



example6 :: Grid
example6 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

example7 :: Grid
example7 = [[0,0,0,0,0,0,0,0,0],
            [0,0,2,3,0,0,0,0,0],
            [0,4,5,6,0,0,0,0,0],
            [0,7,8,9,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0]]