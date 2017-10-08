-- ################################################
-- # Mees Kalf
-- # Excersize 4
-- # 3 hour(s)
-- ################################################
module Ex4 where

import Data.List
import System.Random
import Lecture5


-- Generate a list of all blocks containing their positions
blockPos :: [[(Row, Column)]]
blockPos = concat $ map (\x -> map (\y -> [(i,j)|i <- y ,j <- x]) blocks) blocks

-- Get al different sequences of blocks with a certain length
diffSeq :: Int -> [[(Row, Column)]] -> [[[(Row, Column)]]]
diffSeq n bp = filter ((== n).length) (subsequences bp)

-- Generate an mimimal sudoku with n empty blocks
genSudEmpty :: Int -> IO ()
genSudEmpty n = do 
        [r] <- rsolveNs [emptyN]
        bp <- randomize blockPos
        let tr = map ((foldl eraseN r).concat) (diffSeq n bp)
        let node = take 1 (filter (snd) (zip tr (map uniqueSol tr)))
        if null node
        then print "No minimal solution found"
        else showNode (fst(head(node)))

-- All sudokus atleast have the possibility to have 3 empty blocks, 4 is most times
-- possible and 5 empty blocks are mathemathical impossible. As is explained at:
-- https://puzzling.stackexchange.com/questions/309/what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have
main = do
    genSudEmpty 1
    genSudEmpty 2
    genSudEmpty 3
    genSudEmpty 4
    genSudEmpty 5