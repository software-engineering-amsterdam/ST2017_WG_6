{-
Assignment:		Lab 5: Exercise 6
Name:           Tim Nederveen
Time spent:     5h

Remarks:        
Sources:        - Difficulty Rating of Sudoku Puzzles: An Overview and Evaluation, Pelanek, 2014 (https://arxiv.org/abs/1403.7373)
                - Difficulty Rating of Sudoku Puzzles by a Computational Model, Pelanek, 2014 (https://www.aaai.org/ocs/index.php/FLAIRS/FLAIRS11/paper/download/2517/3077)
---------------}

module Exc6 where

import Data.List
import Data.List.Split
import Lecture5
-- import lab5_3

{-
Exercise 6 (Bonus)

Can you find a way of classifying the difficulty of a Sudoku problem? Can you modify the Sudoku problem generator so that it can 
generate problems that are minimal, but easy to solve by hand? Problems that are minimal but hard to solve by hand? 
How can you test whether the problems your program generates satisfy these properties?
-}


{-
Ranking sudoku difficulty is a non-trivial, and partially subjective task. 
Different solvers will struggle with different techniques or patterns, but [Pelanek, 2014] lists 2 main sources of problem difficulty:
- complexity of individual steps (logic operations)
- structure of dependency among steps

Humans naturally apply Constraint Propagation to solve Sudokus, so to estimate difficulty we prefer this method over backtracking search. 
To know how to rank sudokus as humans would, we will need comparison of some computer-generated metrics to human performance. As Pelanek
has done this for us with a much larger data pool than we can generate ourselves, we will use metrics that have been proven to correlate
with human performance in that research. 

Generating an easy sudoku can be done by checking if the solution can be derived by solely using the Naked Single Technique and the 
Hidden Single Technique. Pelanek shows how dependency and the number of possibilities for each next step also correlates with problem difficulty, 
higher numbers indicating harder problems. Although there are many other metrics, we choose our 
prediction based on this metric, by calculating the possible values per constraint.

To create a threefold classification (easy, medium, hard), we need to perform the following steps:
- Generate a large number of example problems 
- For each problem, output the total sum of the number of next-step possibilities for each field
- Split the outputs into 3 evenly sized groups based on number possibilities. 
  The boundaries of these groups will act as the classification boundaries for easy, medium, and hard

To test whether a newly generated problem satisfies the properties, we test
1. If the sum of next-step possibilities falls within the desired class
2. If it is a minimal problem using the checkUniqueSolution function as defined in exercise 3

-}





-- exampleConstraints :: [Constraint]
-- exampleConstraints = [(2,2,[2,8]),(3,1,[2,4]),(4,2,[4,8]),(6,7,[3,4]),(6,9,[4,7]),(7,5,[2,9]),(1,4,[2,3,7]),(2,3,[2,7,8]),(2,6,[2,3,9]),(2,8,[3,7,9]),(3,4,[2,7,9]),(3,6,[2,6,9]),(4,3,[4,5,8]),(4,4,[3,4,9]),(5,3,[2,4,5]),(6,6,[2,3,4]),(6,8,[3,4,7]),(8,1,[2,4,5]),(8,5,[2,7,9]),(8,8,[4,5,9]),(9,2,[2,4,8]),(9,5,[2,3,9]),(9,8,[4,5,9]),(1,2,[2,4,6,8]),(1,5,[2,3,6,7]),(1,7,[2,3,4,5]),(1,8,[3,4,5,7]),(2,9,[2,7,8,9]),(3,7,[1,2,4,9]),(3,8,[1,4,7,9]),(3,9,[2,4,7,9]),(4,1,[3,4,5,8]),(4,7,[3,4,5,9]),(4,9,[4,5,6,9]),(5,1,[2,3,4,5]),(5,4,[2,3,4,9]),(5,5,[2,3,6,9]),(5,9,[4,5,6,9]),(7,2,[1,2,4,8]),(7,4,[1,2,4,9]),(7,7,[2,4,5,9]),(8,6,[2,4,5,9]),(8,9,[2,4,5,9]),(9,1,[2,4,5,8]),(1,3,[2,4,6,7,8]),(1,9,[2,4,5,7,8]),(5,6,[2,3,4,6,9]),(5,7,[1,3,4,5,9]),(7,3,[2,4,5,8,9]),(7,6,[2,4,5,8,9]),(8,3,[2,4,5,6,9]),(8,4,[1,2,4,7,9]),(9,3,[2,4,5,8,9]),(9,6,[2,3,4,5,8,9])]

-- 
groupContraintsLength :: [Constraint] -> [(Int,Int)]
groupContraintsLength xs = sort (frequency ls)
  where ls = map (\(_,_,vs) -> length vs) xs

-- 
frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (group (sort list))

-- 
getContraintsDifficultyLevel :: [Constraint] -> Int
getContraintsDifficultyLevel cs = foldl (\c (a,l) -> c + a * l) 0 ls
  where ls = groupContraintsLength cs

-- 
gatherDifficultyData :: Int -> [(Int,Int)] -> IO ([(Int,Int)])
gatherDifficultyData 0 rs = return rs
gatherDifficultyData n rs =
  do
    [r] <- rsolveNs [emptyN]
    s <- genProblem r
    let
     cs = snd s
     dl = getContraintsDifficultyLevel cs
    gatherDifficultyData (n - 1) ((dl, length cs) : rs)

difficultyData :: [(Int,Int)]
difficultyData = [(222,57),(250,59),(212,56),(222,56),(239,58),(216,57),(219,57),(190,55),(213,56),(209,56),(205,55),(213,56),(218,56),(223,57),(218,56),(221,57),(237,59),(221,57),(234,58),(226,57),(208,55),(220,56),(216,57),(237,58),(231,58),(197,55),(213,56),(227,57),(204,56),(224,57),(219,57),(215,56),(246,58),(224,57),(214,56),(224,56),(201,56),(212,56),(220,56),(225,57),(213,56),(218,57),(223,58),(219,57),(221,56),(235,58),(222,56),(231,58),(191,55),(233,57),(209,56),(208,56),(215,57),(240,58),(229,58),(217,56),(211,56),(211,56),(213,56),(240,58),(229,59),(228,57),(224,58),(218,56),(222,56),(223,57),(205,56),(219,57),(210,57),(215,56),(203,55),(239,58),(212,57),(230,58),(222,57),(213,56),(190,54),(216,55),(209,55),(227,58),(240,58),(222,58),(219,57),(226,57),(214,56),(219,56),(241,59),(219,57),(205,55),(236,58),(223,56),(208,56),(232,57),(220,58),(219,56),(196,54),(234,59),(213,57),(202,55),(239,58)]

splitDifficultyData :: [(Int, Int)] -> Int -> [(Int,Int)]
splitDifficultyData ds n = map (\i -> (minimum (dsChunks !! i), maximum (dsChunks !! i))) [0..(n -1)]
  where
      ds2 = sort (map fst ds)
      dsChunks = chunksOf (length ds `div` n) ds2

getSudokuDifficultyLevel :: [Constraint] -> String
getSudokuDifficultyLevel cs
    | d < snd (ds !! 0) = "Easy (" ++ show d ++ ")"
    | d < snd (ds !! 1) = "Middle (" ++ show d ++ ")"
    | otherwise = "Hard (" ++ show d ++ ")"
    where
      ds = splitDifficultyData difficultyData 3
      d = getContraintsDifficultyLevel cs

mainEx6 :: IO ()
mainEx6 = do [r] <- rsolveNs [emptyN]
             showNode r
             s  <- genProblem r
             showNode s
             putStrLn ("Difficulty: " ++ show (getSudokuDifficultyLevel (snd s)))



