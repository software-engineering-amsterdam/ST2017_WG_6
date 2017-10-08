{-
Assignment:   Lab 5: Exercise 6
Name:           Tim Nederveen
Time spent:     6.5h

Remarks:        
Sources:        - Difficulty Rating of Sudoku Puzzles: An Overview and Evaluation, Pelanek, 2014 (https://arxiv.org/abs/1403.7373)
                - Difficulty Rating of Sudoku Puzzles by a Computational Model, Pelanek, 2014 (https://www.aaai.org/ocs/index.php/FLAIRS/FLAIRS11/paper/download/2517/3077)
---------------}

module Exc6 where

import Data.List
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
1. Generate a large number (100) of example problems 
- For each problem, output the total sum of the number of next-step possibilities for each field
- Split the outputs into 3 evenly sized groups based on number possibilities. 
  The boundaries of these groups will act as the classification boundaries for easy, medium, and hard

To test whether a newly generated problem satisfies the properties, we test
1. If the sum of next-step possibilities falls within the desired class
2. If it is a minimal problem using the checkUniqueSolution function as defined in exercise 3

Due to time constraints, it was chosen to only define the global approach and functions needed instead of implementing everything
-}

-- We need the following functions for this exercise:


calcPossibilities :: [Constraint] -> Int
-- When passed an list of constraints, outputs a summation of the next-step possibilities

generatePossibilities :: Int -> [Int]
-- When passed an int n, generates n problems using the problem generator, and appends the result of running calcPossibilities on each of those to a list

defineCategoryBoundaries :: [Int] -> Int -> [(Int,Int)]
-- When passed a list of next-step possibility summations from generatePossibilities 
-- and an int n, returns an array of n upper and lower-bound tuples, representing the boundaries of n categories. N will be 3 for this solution

getDifficultyRating :: [Constraint] -> String
-- When passed a list of constraints, generates whether it is "easy", "medium", or "hard" by running calcPossibilities on it and checking in which boundary it falls

checkUniqueSolution :: Node -> (Row,Column) -> Bool
-- The function defined in exercise3 to check whether a solution is minimal.

main = do
  -- genproblem for which it should hold that
  -- checkUniqueSolution == true && getDifficultyRating == "hard"




