module Lab5 where

import Data.List
import System.Random
import Lecture5

-- Sudoku
a :: (Row, Column) -> Value
a (1,2) = 2
p = initNode example6

sudd = grid2sud example6 
sudd1 = grid2sud example7 

k1 = freeAtPos sudd (2,3)
v = subGrid1 sudd (2,2)
n = solveAndShow example6



l (x:xs) = showNode x

-- bl1 :: Int -> [Int]
-- bl1 x = concat $ filter (elem x) blocks1

-- blocks1 :: [[Int]]
-- blocks1 = [[2..4],[6..8]]

-- subGrid1:: Sudoku -> (Row, Column) -> [Value]
-- subGrid1 s (r,c) = [ s (r',c') | r' <- bl1 r, c' <- bl1 c ]
-- -- printAllNodes [] = show ""
-- printAllNodes = showNode (head (initNode example1))

-- freeInSubgrid1 :: Sudoku -> (Row,Column) -> [Value]
-- freeInSubgrid1 s (r,c) = freeInSeq (subGrid1 s (r,c))

cc = constraints (grid2sud example6)

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