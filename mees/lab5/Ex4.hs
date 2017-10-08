module Ex4 where

import Data.List
import System.Random
import Lecture5

blockPos :: [[(Row, Column)]]
blockPos = concat $ map (\x -> map (\y -> [(i,j)|i <- y ,j <- x]) blocks) blocks

diffSeq :: Int -> [[(Row, Column)]] -> [[[(Row, Column)]]]
diffSeq n bp = filter ((== n).length) (subsequences bp)


--  TODO second let can be [] instead of [(sol, _)] that's why man4 crashes sometimes
man n | n > 4 = print "More than 4 empty blocks is mathematically impossible"
      | otherwise = do 
        [r] <- rsolveNs [emptyN]
        bp <- randomize blockPos
        let tr = map ((foldl eraseN r).concat) (diffSeq n bp)
        let [(sol, _)] = take 1 (filter (snd) (zip tr (map uniqueSol tr)))
        showNode sol
































ex :: Grid
ex =   [[5,9,4,2,6,7,8,1,3],
        [3,1,6,8,4,5,7,2,9],
        [8,7,2,9,1,3,6,4,5],
        [7,4,5,3,2,9,1,8,6],
        [9,8,1,6,7,4,5,3,2],
        [2,6,3,1,5,8,9,7,4],
        [1,5,8,4,9,2,3,6,7],
        [6,2,9,7,3,1,4,5,8],
        [4,3,7,5,8,6,2,9,1]]


example :: Grid
example = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [0,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

-- minSudGen = do 
--     [r] <- rsolveNs [emptyN]
--     node@(sud, constr) <- genProblem r
--     let o = map (\x -> length (take 2 (solveNs (initNode(sud2grid (eraseS sud x)))))) (filledPositions sud)
--     if (length(solveNs [node])) == 1 && all (==2) o
--     then return True
--     else return False

-- mep :: Node -> Bool
-- mep (sud, constr) = null (filter (/=0) (subGrid sud ())

blockCords = [(i,j) | i <- [1,4,7], j <- [1,4,7]]

-- mep :: Node -> [Bool]
-- mep (sud, constr) = map (\x -> (not (null (filter (/=0) (subGrid sud x)))))) blockCords

validPos :: [Node] -> [[(Row, Column)]]
validPos [(sud, constr)] = filter (\x -> (sud (head(x))) /= 0) blockPos

allPos = [(i,j) | i <- positions, j <- positions ]

-- menProblem :: [Node] -> [[(Row, Column)]] -> Int -> [Node]
-- menProblem [] _ num = [emptyN]
-- menProblem n b num = do
--                   let a = map (\ys -> pinimalize n ys) b
--                   let ab = filter (not.null) a
--                   if (not.null) ab && (9 - length( validPos (head(ab)))) == num
--                   then return (head(head(ab)))
--                   else menProblem (head(ab)) (validPos (head(ab))) num

menProblem :: [Node] -> [[(Row, Column)]] -> Int -> Int -> [Node]
menProblem n (b:bs) num d = do
    let a = pinimalize n b
    if (not.null) a
    then if d == num
         then a
         else (menProblem a bs num (d + 1))
    else menProblem n bs num d


pinimalize :: [Node] -> [(Row,Column)] -> [Node]
pinimalize [n] [] = [n]
pinimalize [n] ((r,c):rcs) | uniqueSol n = pinimalize [(eraseN n (r,c))] rcs
                           | otherwise = []

-- main = do [r] <- rsolveNs [emptyN]
          -- showNode r
          -- showNode (menProblem [r] blockPos 4)
          -- print "wer"
          -- let s = menProblem r blockPos 3 1
          -- print "we"
          -- showNode (head(s))

          -- print s
          -- solveShowNs s
          -- solveShowNs [s]



-- menProblem :: [Node] -> [[(Row, Column)]] -> Int -> IO ()
-- menProblem [] _ num = print "Whoop"
-- menProblem n b num = do
--                   let a = fmap (\ys -> pinimalize n ys) b
--                   let ab = filter (not.null) a
--                   if null ab
--                   then print "mep"
--                   else do 
--                     -- menProblem (head(ab)) (validPos (head(ab)))
--                     mapM_ (\x -> menProblem x (validPos x)) ab