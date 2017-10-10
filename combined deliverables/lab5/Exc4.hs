{--
Assignment:		Lab 5: Assignment 4
Name:           Sangam Gupta
Time spent:     8 hours
--}
module Exc4 where
    
import Data.List
import Lecture5

-- Erase complete subgrid
eraseSubGrid :: Node -> [(Row, Column)] -> Node
eraseSubGrid = foldl eraseN

-- All coordinates of a subgrid
subGrid' :: (Int, Int) -> [(Int, Int)]
subGrid' (r,c) = [ (r',c') | r' <- bl r, c' <- bl c ]

createTree :: Eq a => [a] -> Int -> [Tree a]
createTree [] _ = []
createTree _ 0 = []
createTree xs n = [T x ( createTree (delete x xs) (n-1) ) | x <- xs]

-- Top left coordinates of each subgrid
coordinates :: [(Int, Int)]
coordinates = [(x,y) | x <- [1,4,7], y <- [1,4,7]]
{-
    We first build a tree of coordinates from n. where n is the height of the tree and 
    the number of subgrids we will check. E.g n = 4 means we build 4 trees from 1..4 which all
    contain subtrees from 1..9 with a max depth of 4. We do this so the tree is optimized. 
    Searching the whole tree will not give a different result.

    Secondly we traverse the tree (Inorder). Until we have reached the desired number of blank subgrids. 
    We then return the node with n blank subgrid and discontinue the search. 
    If we have not reached the desired number of blanks we keep continuing with 
    its childeren until we either find a node with n blanks or
    when no more unique solution are possible. In the last case we return an empty array.
-}
parent :: Tree (Int,Int) -> Int -> Node -> [Node]
parent (T c xs) l n | not (uniqueSol n') = []
                | l == 1 = [n']
                | otherwise = childeren xs (l-1) n' 
                where n' = eraseSubGrid n (subGrid' c)
    
childeren :: [Tree (Int,Int)] -> Int -> Node -> [Node]
childeren [] _ _ = []
childeren (x:xs) l n | length p == 1 = p -- 1 is the number of problems you want to generate
                     | otherwise = childeren xs l n
                     where p = parent x l n


findSudoku n = do [r] <- rsolveNs [emptyN]
                  tree <- return (take n (createTree coordinates n))
                  s <- return (childeren tree n r)
                  if not (null s) then showNode (head s) else print "No problem found" 

{-after running findsudoku a few times we found that a Sudoku with three blanks will always be found. 
  One with four will sometimes be found and a sudoku with five will never be found. Which is correct since
  a sudoke with 5 empty subgrid is not possible.
-}

test = do print "3 blanks"
          findSudoku 3
          print "4 blanks"
          findSudoku 4
          print "5 blanks"
          findSudoku 5

test7 :: Int -> IO ()
test7 0 = print "++ All self-automated tests passed"
test7 n = do
             [r] <- rsolveNs [emptyN]
             tree <- return (take 5 (createTree coordinates 5))
             s <- return (childeren tree 5 r)
             print n
             if (null s)
             then test7 (n - 1)
             else error ("failed test on: " ++ show n)